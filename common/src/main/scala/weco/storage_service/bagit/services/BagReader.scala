package weco.storage_service.bagit.services

import java.io.InputStream
import weco.storage_service.bagit.models._
import weco.storage_service.verify.{
  ChecksumValue,
  HashingAlgorithm,
  MD5,
  SHA1,
  SHA256,
  SHA512
}
import weco.storage._
import weco.storage.store.Readable
import weco.storage.streaming.InputStreamWithLength

import scala.util.{Failure, Success, Try}

trait BagReader[BagLocation <: Location, BagPrefix <: Prefix[BagLocation]] {
  protected val bagFetch = BagPath("fetch.txt")
  protected val bagInfo = BagPath("bag-info.txt")
  protected val payloadManifest =
    (a: HashingAlgorithm) => BagPath(s"manifest-${a.pathRepr}.txt")
  protected val tagManifest =
    (a: HashingAlgorithm) => BagPath(s"tagmanifest-${a.pathRepr}.txt")

  implicit val readable: Readable[BagLocation, InputStreamWithLength]

  type Stream[T] = InputStream => Try[T]

  def get(bagRoot: BagPrefix): Either[BagUnavailable, Bag] =
    for {
      bagInfo <- loadBagInfo(bagRoot)

      payloadManifestResult <- loadPayloadManifest(bagRoot)
      tagManifestResult <- loadTagManifest(bagRoot)

      _ <- {
        val (payloadAlgorithms, _) = payloadManifestResult
        val (tagAlgorithms, _) = tagManifestResult
        compareManifestAlgorithms(payloadAlgorithms, tagAlgorithms)
      }

      bagFetch <- loadBagFetch(bagRoot)

      bag = {
        val (_, payloadManifest) = payloadManifestResult
        val (_, tagManifest) = tagManifestResult

        Bag(bagInfo, payloadManifest, tagManifest, bagFetch)
      }
    } yield bag

  private def loadBagInfo(bagRoot: BagPrefix): Either[BagUnavailable, BagInfo] =
    loadRequired[BagInfo](bagRoot)(bagInfo)(BagInfoParser.create)

  private def loadBagFetch(
    bagRoot: BagPrefix
  ): Either[BagUnavailable, Option[BagFetch]] =
    loadOptional[BagFetch](bagRoot)(bagFetch)(BagFetch.create)

  type ManifestEntries = Map[BagPath, ChecksumValue]

  // RFC 8493 § 3:
  //
  //    For BagIt 1.0, every payload file MUST be listed in every payload
  //    manifest.  Note that older versions of BagIt allowed payload
  //    files to be listed in just one of the manifests.
  //
  private def loadPayloadManifest(
    bagRoot: BagPrefix
  ): Either[BagUnavailable, (Seq[HashingAlgorithm], NewPayloadManifest)] =
    loadManifestEntries(bagRoot, payloadManifest) match {
      case Right((algorithms, entries)) =>
        Right((algorithms, NewPayloadManifest(entries)))
      case Left(ManifestError.CannotBeLoaded(err)) => Left(err)
      case Left(ManifestError.NoManifests) =>
        Left(
          BagUnavailable("Could not find any payload manifests in the bag")
        )
      case Left(ManifestError.InconsistentFilenames) =>
        Left(
          BagUnavailable(
            "Payload manifests are inconsistent: every payload file must be listed in every payload manifest"
          )
        )
      case Left(ManifestError.OnlyWeakChecksums) =>
        Left(
          BagUnavailable(
            "Payload manifests only use weak checksums: add a payload manifest using SHA-256 or SHA-512"
          )
        )
      case Left(_) =>
        Left(
          BagUnavailable("Unknown error while trying to read payload manifests")
        )
    }

  // RFC 8493 § 2.2.1:
  //
  //    A bag MAY contain one or more tag manifests, in which case each tag
  //    manifest SHOULD list the same set of tag files.
  //
  // We interpret this as a MUST.
  private def loadTagManifest(
    bagRoot: BagPrefix
  ): Either[BagUnavailable, (Seq[HashingAlgorithm], NewTagManifest)] =
    loadManifestEntries(bagRoot, tagManifest) match {
      case Right((algorithms, entries)) =>
        Right((algorithms, NewTagManifest(entries)))
      case Left(ManifestError.CannotBeLoaded(err)) => Left(err)
      case Left(ManifestError.NoManifests) =>
        Left(BagUnavailable("Could not find any tag manifests in the bag"))
      case Left(ManifestError.InconsistentFilenames) =>
        Left(
          BagUnavailable(
            "Tag manifests are inconsistent: each tag manifest should list the same set of tag files"
          )
        )
      case Left(ManifestError.OnlyWeakChecksums) =>
        Left(
          BagUnavailable(
            "Tag manifests only use weak checksums: add a tag manifest using SHA-256 or SHA-512"
          )
        )
      case Left(_) =>
        Left(
          BagUnavailable("Unknown error while trying to read tag manifests")
        )
    }

  // RFC 8493 § 2.2.1:
  //
  //    Tag manifests SHOULD use the same algorithms as the payload manifests
  //    that are present in the bag.
  //
  // We interpret this as a MUST.
  private def compareManifestAlgorithms(
    payload: Seq[HashingAlgorithm],
    tag: Seq[HashingAlgorithm]
  ): Either[BagUnavailable, Unit] =
    if (payload == tag) {
      Right(())
    } else {
      Left(
        BagUnavailable(
          "Manifests are inconsistent: tag manifests should use the same algorithms as the payload manifests in the bag"
        )
      )
    }

  private sealed trait ManifestError
  private object ManifestError {
    case class CannotBeLoaded(err: BagUnavailable) extends ManifestError
    case object NoManifests extends ManifestError
    case object InconsistentFilenames extends ManifestError
    case object OnlyWeakChecksums extends ManifestError
    case class UnknownError(t: Throwable) extends ManifestError
  }

  private def loadManifestEntries(
    root: BagPrefix,
    filename: HashingAlgorithm => BagPath
  ): Either[
    ManifestError,
    (Seq[HashingAlgorithm], Map[BagPath, MultiChecksumValue[ChecksumValue]])
  ] =
    for {
      md5 <- loadSingleManifest(root, filename(MD5))
      sha1 <- loadSingleManifest(root, filename(SHA1))
      sha256 <- loadSingleManifest(root, filename(SHA256))
      sha512 <- loadSingleManifest(root, filename(SHA512))

      manifests <- Seq(md5, sha1, sha256, sha512).flatten match {
        case Nil   => Left(ManifestError.NoManifests)
        case other => Right(other)
      }

      algorithms = Seq(
        md5.map(_ => MD5),
        sha1.map(_ => SHA1),
        sha256.map(_ => SHA256),
        sha512.map(_ => SHA512)
      ).flatten

      filenames <- manifests.map(_.keys.toSeq).distinct match {
        case Seq(names) => Right(names)
        case _          => Left(ManifestError.InconsistentFilenames)
      }

      entries <- Try {
        filenames.map { bagPath =>
          bagPath -> MultiChecksumValue(
            md5 = md5.map(_(bagPath)),
            sha1 = sha1.map(_(bagPath)),
            sha256 = sha256.map(_(bagPath)),
            sha512 = sha512.map(_(bagPath))
          )
        }.toMap
      } match {
        case Success(entries) => Right(entries)
        case Failure(MultiChecksumException.OnlyWeakChecksums) =>
          Left(ManifestError.OnlyWeakChecksums)
        case Failure(t) => Left(ManifestError.UnknownError(t))
      }
    } yield (algorithms, entries)

  private def loadSingleManifest(
    root: BagPrefix,
    path: BagPath
  ): Either[ManifestError.CannotBeLoaded, Option[ManifestEntries]] =
    loadOptional[ManifestEntries](root)(path)(BagManifestParser.parse) match {
      case Right(entries) => Right(entries)
      case Left(bagUnavailable) =>
        Left(ManifestError.CannotBeLoaded(bagUnavailable))
    }

  private def loadOptional[T](
    root: BagPrefix
  )(path: BagPath)(f: Stream[T]): Either[BagUnavailable, Option[T]] = {
    val location = root.asLocation(path.value)

    readable.get(location) match {
      case Right(stream) =>
        f(stream.identifiedT) match {
          case Success(r) => Right(Some(r))
          case Failure(e) =>
            Left(
              BagUnavailable(s"Error loading ${path.value}: ${e.getMessage}")
            )
        }

      case Left(_: DoesNotExistError) =>
        Right(None)

      case Left(err) =>
        Left(BagUnavailable(s"Error loading ${path.value}: $err"))
    }
  }

  private def loadRequired[T](
    root: BagPrefix
  )(path: BagPath)(f: Stream[T]): Either[BagUnavailable, T] =
    loadOptional[T](root)(path)(f) match {
      case Right(Some(result)) => Right(result)
      case Right(None) =>
        Left(BagUnavailable(s"Error loading ${path.value}: no such file!"))
      case Left(err) => Left(err)
    }
}
