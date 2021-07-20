package weco.storage_service.indexer.files.models

import java.time.Instant

import weco.storage_service.bagit.models.ExternalIdentifier
import weco.storage_service.storage.models.StorageSpace
import weco.storage_service.indexer.models.FileContext
import weco.storage.s3.S3ObjectLocation

case class IndexedFile(
  space: StorageSpace,
  externalIdentifier: ExternalIdentifier,
  location: S3ObjectLocation,
  name: String,
  suffix: Option[String],
  size: Long,
  checksum: IndexedChecksum,
  createdDate: Instant
)

case object IndexedFile {
  def apply(context: FileContext): IndexedFile =
    IndexedFile(
      space = context.space,
      externalIdentifier = context.externalIdentifier,
      location = context.bagLocation.prefix.asLocation(context.file.path),
      suffix = FileSuffix.getSuffix(context.file.name),
      name = context.file.name,
      size = context.file.size,
      checksum = IndexedChecksum(
        algorithm = context.algorithm.toString,
        value = context.file.checksum.toString
      ),
      createdDate = context.createdDate
    )
}
