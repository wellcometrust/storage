package weco.storage_service.bag_register.services

import java.net.URI
import java.time.Instant
import com.amazonaws.services.s3.AmazonS3
import grizzled.slf4j.Logging
import weco.storage_service.bagit.models._
import weco.storage_service.bagit.services.BagMatcher
import weco.storage_service.ingests.models.IngestID
import weco.storage_service.storage.models._
import weco.storage._
import weco.storage.azure.AzureBlobLocation
import weco.storage.s3.{S3ObjectLocation, S3ObjectLocationPrefix}
import weco.storage.services.SizeFinder
import weco.storage.services.s3.S3SizeFinder
import weco.storage.store.Readable
import weco.storage.store.s3.S3StreamStore
import weco.storage.streaming.InputStreamWithLength
import weco.storage_service.verify.{HashingAlgorithm, SHA256, SHA512}

import scala.util.{Failure, Success, Try}

class S3StorageManifestService(implicit s3Client: AmazonS3) extends Logging {
  val sizeFinder: SizeFinder[S3ObjectLocation] =
    new S3SizeFinder()

  implicit val streamReader: Readable[S3ObjectLocation, InputStreamWithLength] =
    new S3StreamStore()

  private lazy val tagManifestFileFinder = new TagManifestFileFinder()

  def createLocation(uri: URI): S3ObjectLocation =
    new S3ObjectLocation(
      bucket = uri.getHost,
      key = uri.getPath.stripPrefix("/")
    )

  def createManifest(
    ingestId: IngestID,
    bag: Bag,
    location: PrimaryReplicaLocation,
    replicas: Seq[SecondaryReplicaLocation],
    space: StorageSpace,
    version: BagVersion
  ): Try[StorageManifest] =
    for {
      bagRoot <- getBagRoot(
        location.prefix.asInstanceOf[S3ObjectLocationPrefix],
        version
      ).map { _.asInstanceOf[S3ObjectLocationPrefix] }

      replicaLocations <- getReplicaLocations(replicas, version)

      matchedLocations <- resolveFetchLocations(bag)

      entries <- createPathLocationMap(
        matchedLocations = matchedLocations,
        bagRoot = bagRoot,
        version = version
      )

      algorithm <- chooseAlgorithm(bag.tagManifest)

      fileManifestFiles <- createManifestFiles(
        bagRoot = bagRoot,
        algorithm = algorithm,
        manifest = bag.manifest,
        entries = entries
      )

      tagManifestFiles <- createManifestFiles(
        bagRoot = bagRoot,
        algorithm = algorithm,
        manifest = bag.tagManifest,
        entries = entries
      )

      unreferencedTagManifestFiles <- getUnreferencedFiles(
        bagRoot = bagRoot,
        version = version,
        algorithm = algorithm
      )

      storageManifest = StorageManifest(
        space = space,
        info = bag.info,
        version = version,
        manifest = FileManifest(
          checksumAlgorithm = algorithm,
          files = fileManifestFiles
        ),
        tagManifest = FileManifest(
          checksumAlgorithm = algorithm,
          files = tagManifestFiles ++ unreferencedTagManifestFiles
        ),
        location = PrimaryStorageLocation(bagRoot),
        replicaLocations = replicaLocations,
        createdDate = Instant.now,
        ingestId = ingestId
      )
    } yield storageManifest

  /** Currently the StorageManifest model only supports a single algorithm, but
    * a bag can contain multiple manifest files for different algorithms.
    *
    * Choose the algorithm to use for this storage manifest.
    *
    * Implementation notes:
    *
    *   - We know the tag manifest is empty because it has to describe
    *     the bag declaration (bagit.txt).  This means we can call `.head` with
    *     impunity, and be sure a file exists.
    *   - The presence of a "strong" algorithm (i.e. not MD5 or SHA-1) is checked
    *     by the BagReader. class
    *
    */
  private def chooseAlgorithm(
    tagManifest: NewTagManifest
  ): Try[HashingAlgorithm] = {
    val (_, checksum) = tagManifest.entries.head

    val algorithms =
      Seq(SHA512, SHA256)
        .map { ha =>
          (ha, checksum.getValue(ha))
        }
        .collect { case (ha, Some(_)) => ha }

    algorithms.headOption match {
      case Some(algo) => Success(algo)
      case None =>
        Failure(
          new Throwable(
            s"Unable to choose algorithm from tag manifest: $tagManifest"
          )
        )
    }
  }

  // TODO: Upstream into scala-libs
  def getPath(location: Location): String =
    location match {
      case s3Location: S3ObjectLocation     => s3Location.key
      case azureLocation: AzureBlobLocation => azureLocation.name
      case _                                => throw new Throwable(s"Unsupported location: $location")
    }

  /** The replicator writes bags inside a bucket to paths of the form
    *
    *     /{storageSpace}/{externalIdentifier}/v{version}
    *
    * All the versions of a bag should follow this convention, so if we
    * strip off the /:version prefix we'll find the root of all bags
    * with this (storage space, external ID) pair.
    *
    * TODO: It would be better if we passed a structured object out of
    * the replicator.
    *
    */
  private def getBagRoot(
    replicaRoot: Prefix[_ <: Location],
    version: BagVersion
  ): Try[Prefix[_ <: Location]] =
    if (replicaRoot.basename == version.toString) {
      Success(replicaRoot.parent)
    } else {
      Failure(
        new StorageManifestException(
          s"Malformed bag root: $replicaRoot (expected suffix /$version)"
        )
      )
    }

  private def resolveFetchLocations(bag: Bag): Try[Seq[MatchedLocation]] =
    BagMatcher.correlateFetchEntries(bag) match {
      case Right(matchedLocations) => Success(matchedLocations)
      case Left(err) =>
        Failure(
          new StorageManifestException(
            s"Unable to resolve fetch entries: $err"
          )
        )
    }

  private def getSizeAndLocation(
    matchedLocation: MatchedLocation,
    bagRoot: S3ObjectLocationPrefix,
    version: BagVersion
  ): (S3ObjectLocation, Option[Long]) =
    matchedLocation.fetchMetadata match {
      // A concrete file inside the replicated bag, so it's inside
      // the versioned replica directory.
      case None =>
        (
          bagRoot.asLocation(version.toString, matchedLocation.bagPath.value),
          None
        )

      // An entry in the fetch.txt, referring to a file somewhere else.
      case Some(fetchMetadata) =>
        (
          createLocation(fetchMetadata.uri),
          fetchMetadata.length
        )
    }

  /** Every entry in the bag manifest will be either a:
    *
    *   - concrete file inside the replicated bag, or
    *   - a file referenced by the fetch file, which should be in a different
    *     versioned directory under the same bag root
    *
    * The map contains data
    *
    *   (bag path) -> (location, size if known)
    *
    */
  private def createPathLocationMap(
    matchedLocations: Seq[MatchedLocation],
    bagRoot: S3ObjectLocationPrefix,
    version: BagVersion
  ): Try[Map[BagPath, (S3ObjectLocation, Option[Long])]] =
    Try {
      matchedLocations.map { matchedLoc =>
        (
          matchedLoc.bagPath,
          getSizeAndLocation(matchedLoc, bagRoot, version)
        )
      }.toMap
    }

  private def createManifestFiles(
    manifest: NewBagManifest,
    algorithm: HashingAlgorithm,
    entries: Map[BagPath, (S3ObjectLocation, Option[Long])],
    bagRoot: S3ObjectLocationPrefix
  ): Try[Seq[StorageManifestFile]] = Try {
    manifest.entries.map {
      case (bagPath, multiChecksum) =>
        // This lookup should never file -- the BagMatcher populates the
        // entries from the original manifests in the bag.
        //
        // The bag verifier should already have checked that any entries in
        // the fetch.txt are inside the bag in the storage service.
        //
        // We wrap it in a Try block just in case, but this should never
        // throw in practice.
        val (location, maybeSize) = entries(bagPath)

        assert(
          getPath(location).startsWith(bagRoot.pathPrefix + "/"),
          s"Looks like a fetch.txt URI wasn't under the bag root - why wasn't this spotted by the verifier? +" +
            s"$location ($bagPath)"
        )
        val path = getPath(location).stripPrefix(bagRoot.pathPrefix + "/")

        val size = maybeSize match {
          case Some(s) => s
          case None =>
            sizeFinder.getSize(location) match {
              case Right(value) => value
              case Left(readError) =>
                throw new StorageManifestException(
                  s"Error getting size of $location: ${readError.e}"
                )
            }
        }

        // This lookup should never fail -- we should already have validated in
        // the BagReader class and the bag verifier that if an algorithm is used,
        // it is used to describe every file in the bag.
        val checksum = multiChecksum.getValue(algorithm).get

        StorageManifestFile(
          checksum = checksum,
          name = bagPath.value,
          path = path,
          size = size
        )
    }.toSeq
  }

  // Get StorageManifestFile entries for everything not listed in either the manifest
  // or tag manifest BagIt files.
  //
  // This should only be the tagmanifest-*.txt files -- the verifier checks that these
  // are the only unreferenced files.
  //
  private def getUnreferencedFiles(
    bagRoot: S3ObjectLocationPrefix,
    version: BagVersion,
    algorithm: HashingAlgorithm
  ): Try[Seq[StorageManifestFile]] =
    tagManifestFileFinder
      .getTagManifestFiles(
        // TODO: Upstream a join() method into scala-libs
        prefix = bagRoot.asLocation(version.toString).asPrefix,
        algorithm = algorithm
      )
      .map {
        // Remember to prefix all the entries with a version string
        _.map { f =>
          f.copy(path = s"$version/${f.path}")
        }
      }

  private def getReplicaLocations(
    replicas: Seq[SecondaryReplicaLocation],
    version: BagVersion
  ): Try[Seq[SecondaryStorageLocation]] = {
    val rootReplicas =
      replicas
        .map { loc =>
          getBagRoot(replicaRoot = loc.prefix, version = version)
            .map { SecondaryStorageLocation(_) }
        }

    val successes = rootReplicas.collect { case Success(loc) => loc }
    val failures = rootReplicas.collect { case Failure(err)  => err }

    if (failures.isEmpty) {
      Success(successes)
    } else {
      Failure(
        new StorageManifestException(
          s"Malformed bag root in the replicas: $failures"
        )
      )
    }
  }
}
