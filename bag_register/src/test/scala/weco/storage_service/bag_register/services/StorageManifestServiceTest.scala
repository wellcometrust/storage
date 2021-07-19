package weco.storage_service.bag_register.services

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, TryValues}
import weco.storage_service.bagit.models.{
  Bag,
  BagPath,
  BagVersion,
  MultiChecksumValue
}
import weco.storage_service.bagit.services.s3.S3BagReader
import weco.storage_service.fixtures.PayloadEntry
import weco.storage_service.fixtures.s3.S3BagBuilder
import weco.storage_service.generators._
import weco.storage_service.ingests.models.IngestID
import weco.storage_service.storage.models._
import weco.storage._
import weco.storage.fixtures.S3Fixtures
import weco.storage.fixtures.S3Fixtures.Bucket
import weco.storage.s3.{S3ObjectLocation, S3ObjectLocationPrefix}
import weco.storage.services.s3.S3SizeFinder
import weco.fixtures.TimeAssertions

import scala.util.Random

class StorageManifestServiceTest
    extends AnyFunSpec
    with Matchers
    with BagGenerators
    with FetchMetadataGenerators
    with ReplicaLocationGenerators
    with StorageSpaceGenerators
    with TimeAssertions
    with TryValues
    with S3Fixtures {

  describe("checks the replica root paths") {
    describe("primary location") {
      it("fails if the root is not a versioned directory") {
        val location = PrimaryS3ReplicaLocation(createS3ObjectLocationPrefix)
        val version = createBagVersion

        assertIsError(location = location, version = version) { err =>
          err shouldBe a[StorageManifestException]
          err.getMessage shouldBe s"Malformed bag root: ${location.prefix} (expected suffix /$version)"
        }
      }

      it("fails if the root has the wrong version") {
        val version = createBagVersion
        val location = createPrimaryLocationWith(version = version.increment)

        assertIsError(location = location, version = version) { err =>
          err shouldBe a[StorageManifestException]
          err.getMessage shouldBe s"Malformed bag root: ${location.prefix} (expected suffix /$version)"
        }
      }
    }

    describe("secondary location") {
      it("fails if the root is not a versioned directory") {
        val replicas = Seq(
          createSecondaryLocation,
          createSecondaryLocation
        )

        assertIsError(replicas = replicas) { err =>
          err shouldBe a[StorageManifestException]
          err.getMessage should startWith("Malformed bag root in the replicas:")
        }
      }

      it("fails if the root has the wrong version") {
        val location = createPrimaryLocationWith(version = BagVersion(1))
        val version = BagVersion(1)

        val replicas = Seq(
          createSecondaryLocationWith(BagVersion(2)),
          createSecondaryLocationWith(BagVersion(3))
        )

        assertIsError(
          location = location,
          replicas = replicas,
          version = version
        ) { err =>
          err shouldBe a[StorageManifestException]
          err.getMessage should startWith("Malformed bag root in the replicas:")
        }
      }
    }
  }

  it("sets the locations and replicaLocations correctly") {
    val version = createBagVersion

    withLocalS3Bucket { implicit bucket =>
      val (bagRoot, bag) = createStorageManifestBag(version = version)

      val location = PrimaryS3ReplicaLocation(bagRoot)

      val replicas = collectionOf(max = 10) {
        createSecondaryLocationWith(version)
      }

      val storageManifest = createManifest(
        bag = bag,
        location = location,
        replicas = replicas,
        version = version
      )

      storageManifest.location.prefix shouldBe bagRoot
        .copy(
          keyPrefix = bagRoot.keyPrefix.stripSuffix(s"/$version")
        )

      val expectedPrefixes = replicas
        .map { _.prefix.parent }

      val actualPrefixes = storageManifest.replicaLocations.map { _.prefix }

      actualPrefixes shouldBe expectedPrefixes
    }
  }

  it("if there's no fetch.txt, it versions the paths") {
    object NoFetchBagBuilder extends S3BagBuilder {
      override protected def getFetchEntryCount(payloadFileCount: Int): Int = 0
    }

    val version = createBagVersion

    withLocalS3Bucket { implicit bucket =>
      val (bagRoot, bag) = createStorageManifestBag(
        version = version,
        bagBuilder = NoFetchBagBuilder
      )

      val location = PrimaryS3ReplicaLocation(bagRoot)

      val storageManifest = createManifest(
        bag = bag,
        location = location,
        version = version
      )

      storageManifest.manifest.files
        .foreach { file =>
          file.path shouldBe s"$version/${file.name}"
        }

      storageManifest.tagManifest.files
        .foreach { file =>
          file.path shouldBe s"$version/${file.name}"
        }
    }
  }

  it("if there's a fetch.txt, it constructs the right paths") {
    object AtLeastOneFetchEntryBagBuilder extends S3BagBuilder {
      override protected def getFetchEntryCount(payloadFileCount: Int): Int =
        randomInt(from = 1, to = payloadFileCount)
    }

    val version = createBagVersion

    withLocalS3Bucket { implicit bucket =>
      val (bagRoot, bag) = createStorageManifestBag(
        version = version,
        bagBuilder = AtLeastOneFetchEntryBagBuilder
      )

      val location = PrimaryS3ReplicaLocation(bagRoot)

      val storageManifest = createManifest(
        bag = bag,
        location = location,
        version = version
      )

      val bagFetchEntries = bag.fetch.get.entries

      // it puts fetched entries under a versioned path
      val fetchedFiles = storageManifest.manifest.files
        .filter { file =>
          bagFetchEntries.contains(BagPath(file.name))
        }

      fetchedFiles should not be empty

      fetchedFiles
        .foreach { file =>
          val fetchEntry = bagFetchEntries(BagPath(file.name))

          // The fetch entry URI is of the form
          //
          //    s3://{bucket}/{space}/{externalIdentifier}/{version}/{path_inside_bag}
          //
          // We want to check that matches the bag path, which is of the form
          //
          //    {version}/{path_inside_bag}
          //
          fetchEntry.uri.toString should endWith(file.path)
          file.path.matches("^v\\d+/")
        }

      // it puts non-fetched entries under the current version
      storageManifest.manifest.files
        .filterNot { file =>
          bagFetchEntries.contains(BagPath(file.name))
        }
        .foreach { file =>
          file.path shouldBe s"$version/${file.name}"
        }

      // tag manifest entries are always under the current version
      storageManifest.tagManifest.files
        .foreach { file =>
          file.path shouldBe s"$version/${file.name}"
        }
    }
  }

  it("uses the correct checksum values") {
    val version = createBagVersion

    withLocalS3Bucket { implicit bucket =>
      val (bagRoot, bag) = createStorageManifestBag(version = version)

      val location = PrimaryS3ReplicaLocation(bagRoot)

      val storageManifest = createManifest(
        bag = bag,
        location = location,
        version = version
      )

      // in the file manifest
      val storageManifestChecksums =
        storageManifest.manifest.files.map { file =>
          file.name -> file.checksum
        }.toMap

      val bagChecksums =
        bag.manifest.entries
          .map {
            case (bagPath, multiChecksum) =>
              bagPath.value -> multiChecksum.sha256.get
          }

      storageManifestChecksums shouldBe bagChecksums

      // in the tag manifest
      val tagManifestChecksums =
        storageManifest.tagManifest.files.map { file =>
          file.name -> file.checksum
        }.toMap

      val tagChecksums =
        bag.tagManifest.entries
          .map {
            case (bagPath, multiChecksum) =>
              bagPath.value -> multiChecksum.sha256.get
          }

      tagManifestChecksums.filterKeys { _ != "tagmanifest-sha256.txt" } shouldBe tagChecksums
    }
  }

  it("prefers SHA-512 checksums to SHA-256") {
    val version = createBagVersion

    withLocalS3Bucket { implicit bucket =>
      val (bagRoot, bag) = createStorageManifestBag(version = version)

      val bagWithSha512 = bag.copy(
        manifest = bag.manifest.copy(
          entries = bag.manifest.entries
            .map {
              case (path, multiChecksum) =>
                (path, multiChecksum.copy(sha512 = Some(randomChecksumValue)))
            }
        ),
        tagManifest = bag.tagManifest.copy(
          entries = bag.tagManifest.entries
            .map {
              case (path, multiChecksum) =>
                (path, multiChecksum.copy(sha512 = Some(randomChecksumValue)))
            }
        )
      )

      val location = PrimaryS3ReplicaLocation(bagRoot)

      val storageManifest = createManifest(
        bag = bagWithSha512,
        location = location,
        version = version
      )

      // in the file manifest
      val storageManifestChecksums =
        storageManifest.manifest.files.map { file =>
          file.name -> file.checksum
        }.toMap

      val bagChecksums =
        bagWithSha512.manifest.entries
          .map {
            case (bagPath, multiChecksum) =>
              bagPath.value -> multiChecksum.sha512.get
          }

      storageManifestChecksums shouldBe bagChecksums

      // in the tag manifest
      val tagManifestChecksums =
        storageManifest.tagManifest.files.map { file =>
          file.name -> file.checksum
        }.toMap

      val tagChecksums =
        bagWithSha512.tagManifest.entries
          .map {
            case (bagPath, multiChecksum) =>
              bagPath.value -> multiChecksum.sha512.get
          }

      tagManifestChecksums.filterKeys { _ != "tagmanifest-sha256.txt" } shouldBe tagChecksums
    }
  }

  describe("fails if the fetch.txt is wrong") {
    it("refers to files that aren't in the manifest") {
      val fetchEntries = Map(
        createBagPath -> createFetchMetadata
      )

      val bag = createBagWith(
        fetchEntries = fetchEntries
      )

      assertIsError(bag = bag) { err =>
        err shouldBe a[StorageManifestException]
        err.getMessage should startWith("Unable to resolve fetch entries:")
        err.getMessage should include(
          s"fetch.txt refers to paths that aren't in the bag manifest:"
        )
      }
    }
  }

  it("passes through metadata correctly") {
    val version = createBagVersion
    val space = createStorageSpace

    withLocalS3Bucket { implicit bucket =>
      val (bagRoot, bag) = createStorageManifestBag(version = version)

      val location = PrimaryS3ReplicaLocation(bagRoot)

      val storageManifest = createManifest(
        bag = bag,
        location = location,
        space = space,
        version = version
      )

      storageManifest.space shouldBe space
      storageManifest.info shouldBe bag.info
      storageManifest.version shouldBe version

      // This test takes longer when running on a Mac, not in CI, so allow some
      // flex on the definition of "recent".
      assertRecent(storageManifest.createdDate, recentSeconds = 45)
    }
  }

  describe("adds the size to the manifest files") {
    it("fails if it cannot get the size of a file") {
      val version = createBagVersion
      val location = createPrimaryLocationWith(
        version = version
      )

      val files = Seq("data/file1.txt", "data/file2.txt", "data/dir/file3.txt")

      val bag = createBagWith(
        manifestEntries = files.map {
          BagPath(_) -> MultiChecksumValue(sha256 = Some(randomChecksumValue))
        }.toMap,
        tagManifestEntries = Map(
          BagPath("bagit.txt") -> MultiChecksumValue(
            sha256 = Some(randomChecksumValue)
          )
        )
      )

      val err = new Throwable("BOOM!")

      implicit val brokenSizeFinder: S3SizeFinder =
        new S3SizeFinder() {
          override def get(location: S3ObjectLocation): ReadEither =
            Left(StoreReadError(err))
        }

      val service = new S3StorageManifestService() {
        override val sizeFinder: S3SizeFinder = brokenSizeFinder
      }

      val result = service.createManifest(
        ingestId = createIngestID,
        bag = bag,
        location = location,
        replicas = Seq.empty,
        space = createStorageSpace,
        version = version
      )

      result.failed.get shouldBe a[StorageManifestException]
      result.failed.get.getMessage should startWith(
        s"Error getting size of ${location.prefix.asLocation("data/file1.txt")}"
      )
    }

    it("uses the size finder to get sizes") {
      object NoFetchBagBuilder extends S3BagBuilder {
        override protected def getFetchEntryCount(payloadFileCount: Int): Int =
          0
      }

      val version = createBagVersion

      withLocalS3Bucket { implicit bucket =>
        val (bagRoot, bag) = createStorageManifestBag(
          version = version,
          bagBuilder = NoFetchBagBuilder
        )

        val location = PrimaryS3ReplicaLocation(bagRoot)

        var sizeCache: Map[S3ObjectLocation, Long] = Map.empty

        val cachingSizeFinder: S3SizeFinder = new S3SizeFinder() {
          override def get(location: S3ObjectLocation): ReadEither = {
            sizeCache = sizeCache + (location -> Random.nextLong())
            val size = sizeCache(location)
            Right(Identified(location, size))
          }
        }

        val storageManifest = createManifest(
          bag = bag,
          location = location,
          version = version,
          sizeFinderImpl = cachingSizeFinder
        )

        val storageManifestSizes =
          (storageManifest.manifest.files ++ storageManifest.tagManifest.files)
            .filterNot {
              // The size of this tag manifest is fetched when we read the file contents,
              // not from the size finder.
              _.name == "tagmanifest-sha256.txt"
            }
            .map { file =>
              storageManifest.location.prefix.asLocation(file.path) -> file.size
            }
            .toMap

        storageManifestSizes shouldBe sizeCache.map {
          case (cachedLoc, cachedSize) => cachedLoc -> cachedSize
        }
      }
    }

    it("uses the size from the fetch file") {
      // Always create a fetch.txt entry rather than a concrete file, always
      // include the size in the fetch.txt.
      object ConcreteFetchEntryBagBuilder extends S3BagBuilder {
        override protected def getFetchEntryCount(payloadFileCount: Int): Int =
          payloadFileCount

        override def buildFetchEntryLine(
          primaryBucket: Bucket,
          entry: PayloadEntry
        ): String =
          s"""s3://$primaryBucket/${entry.path} ${entry.contents.getBytes.length} ${entry.bagPath}"""
      }

      val version = createBagVersion

      withLocalS3Bucket { implicit bucket =>
        val (bagRoot, bag) = createStorageManifestBag(
          version = version,
          bagBuilder = ConcreteFetchEntryBagBuilder
        )

        val location = PrimaryS3ReplicaLocation(bagRoot)

        val err = new Throwable("This should never be called!")

        val brokenSizeFinder = new S3SizeFinder() {
          override def get(location: S3ObjectLocation): ReadEither =
            location.key match {
              case s if !s.contains("/data/") => Right(Identified(location, 1L))
              case _                          => Left(StoreReadError(err))
            }
        }

        val storageManifest = createManifest(
          bag = bag,
          location = location,
          version = version,
          sizeFinderImpl = brokenSizeFinder
        )

        val manifestSizes =
          storageManifest.manifest.files.map { file =>
            file.name -> file.size
          }.toMap

        val bagSizes = bag.fetch.get.entries.map {
          case (bagPath, fetchMetadata) =>
            bagPath.value -> fetchMetadata.length.get
        }

        manifestSizes shouldBe bagSizes
      }
    }
  }

  it("includes the tag manifest files") {
    val version = createBagVersion

    withLocalS3Bucket { implicit bucket =>
      val (bagRoot, bag) = createStorageManifestBag(version = version)

      val manifest = createManifest(
        bag = bag,
        location = PrimaryS3ReplicaLocation(bagRoot),
        version = version
      )

      val tagManifestFiles = manifest.tagManifest.files.filter {
        _.name.startsWith("tagmanifest-")
      }

      tagManifestFiles should not be empty
    }
  }

  def createStorageManifestBag(
    space: StorageSpace = createStorageSpace,
    version: BagVersion,
    bagBuilder: S3BagBuilder = new S3BagBuilder {}
  )(
    implicit bucket: Bucket
  ): (S3ObjectLocationPrefix, Bag) = {
    val (bagRoot, _) = bagBuilder.storeS3BagWith(
      space = space,
      version = version
    )

    (
      bagRoot,
      new S3BagReader().get(bagRoot).value
    )
  }

  private def createManifest(
    ingestId: IngestID = createIngestID,
    bag: Bag,
    location: PrimaryReplicaLocation,
    replicas: Seq[SecondaryReplicaLocation] = Seq.empty,
    space: StorageSpace = createStorageSpace,
    version: BagVersion,
    sizeFinderImpl: S3SizeFinder = new S3SizeFinder()
  ): StorageManifest = {
    val service: S3StorageManifestService = new S3StorageManifestService() {
      override val sizeFinder: S3SizeFinder = sizeFinderImpl
    }

    val result = service.createManifest(
      ingestId = ingestId,
      bag = bag,
      location = location,
      replicas = replicas,
      space = space,
      version = version
    )

    if (result.isFailure) {
      println(result)
    }

    result.success.value
  }

  def createPrimaryLocationWith(version: BagVersion): PrimaryReplicaLocation =
    createPrimaryLocationWith(
      bagRoot = createS3ObjectLocationPrefix,
      version = version
    )

  def createPrimaryLocationWith(
    bagRoot: S3ObjectLocationPrefix,
    version: BagVersion
  ): PrimaryReplicaLocation =
    PrimaryS3ReplicaLocation(
      // TODO: Add a .join() method to S3ObjectLocationPrefix in scala-libs
      bagRoot.asLocation(version.toString).asPrefix
    )

  def createSecondaryLocationWith(
    version: BagVersion
  ): SecondaryReplicaLocation =
    chooseFrom(
      SecondaryS3ReplicaLocation(
        createS3ObjectLocationPrefix
          .asLocation(version.toString)
          .asPrefix
      ),
      SecondaryAzureReplicaLocation(
        createAzureBlobLocationPrefix
          .asLocation(version.toString)
          .asPrefix
      )
    )

  private def assertIsError(
    ingestId: IngestID = createIngestID,
    bag: Bag = createBag,
    location: PrimaryReplicaLocation = createPrimaryLocationWith(
      version = BagVersion(1)
    ),
    replicas: Seq[SecondaryReplicaLocation] = Seq.empty,
    version: BagVersion = BagVersion(1)
  )(assertError: Throwable => Assertion): Assertion = {
    val service = new S3StorageManifestService()

    val result = service.createManifest(
      ingestId = ingestId,
      bag = bag,
      location = location,
      replicas = replicas,
      space = createStorageSpace,
      version = version
    )

    assertError(result.failure.exception)
  }
}
