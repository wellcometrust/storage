package weco.storage_service.bag_register.services

import weco.storage_service.bagit.models.UnreferencedFiles
import weco.storage_service.storage.models.StorageManifestFile
import weco.storage_service.checksum.{ChecksumAlgorithm, MultiChecksum}
import weco.storage.store.Readable
import weco.storage.streaming.InputStreamWithLength
import weco.storage._

import scala.util.{Failure, Success, Try}

/** The tag manifest files (e.g. tagmanifest-sha256.txt) aren't referred to by
  * any of the other manifests in the bag, but we still want to include them in
  * the storage manifest created by the storage service.
  *
  * This class creates the `StorageManifestFile` entries for BagIt tag manifest files.
  *
  */
class TagManifestFileFinder[BagLocation <: Location](
  implicit streamReader: Readable[BagLocation, InputStreamWithLength]
) {

  def getTagManifestFiles(
    prefix: Prefix[BagLocation],
    algorithm: ChecksumAlgorithm
  ): Try[Seq[StorageManifestFile]] = Try {
    val entries: Seq[StorageManifestFile] =
      UnreferencedFiles.tagManifestFiles.flatMap {
        findIndividualTagManifestFile(_, prefix, algorithm)
      }

    if (entries.isEmpty) {
      throw new RuntimeException(s"No tag manifest files found under $prefix")
    } else {
      entries
    }
  }

  private def findIndividualTagManifestFile(
    name: String,
    prefix: Prefix[BagLocation],
    algorithm: ChecksumAlgorithm
  ): Option[StorageManifestFile] =
    streamReader.get(prefix.asLocation(name)) match {
      case Right(is) =>
        // This method is called in the bag register to create the storage manifest,
        // so it happens after the entire bag has been verified.  To verify the bag,
        // we've already had to read the tagmanifest-sha256.txt file, so an error
        // here would be unlikely (but probably not impossible).
        val checksum = MultiChecksum.create(is.identifiedT) match {
          case Success(hashResult) => hashResult.getValue(algorithm)
          case Failure(err) =>
            throw new RuntimeException(
              s"Error reading tag manifest: $err"
            )
        }

        Some(
          StorageManifestFile(
            checksum = checksum,
            name = name,
            path = name,
            size = is.identifiedT.length
          )
        )

      case Left(_: DoesNotExistError) => None

      case Left(err) =>
        throw new RuntimeException(s"Error looking up $prefix/$name: $err")
    }
}
