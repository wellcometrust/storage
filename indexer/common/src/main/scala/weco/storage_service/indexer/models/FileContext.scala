package weco.storage_service.indexer.models

import weco.storage.s3.S3ObjectLocation
import weco.storage_service.bagit.models.ExternalIdentifier
import weco.storage_service.storage.models.{
  PrimaryS3StorageLocation,
  StorageManifest,
  StorageManifestFile,
  StorageSpace
}
import weco.storage_service.checksum.ChecksumAlgorithm

import java.time.Instant

case class FileContext(
  space: StorageSpace,
  externalIdentifier: ExternalIdentifier,
  algorithm: ChecksumAlgorithm,
  bagLocation: PrimaryS3StorageLocation,
  file: StorageManifestFile,
  createdDate: Instant
) {
  def location: S3ObjectLocation =
    bagLocation.prefix.asLocation(file.path)
}

case object FileContext {
  def apply(
    manifest: StorageManifest,
    file: StorageManifestFile
  ): FileContext = {
    assert(manifest.manifest.files.contains(file))
    FileContext(
      space = manifest.space,
      externalIdentifier = manifest.info.externalIdentifier,
      algorithm = manifest.manifest.checksumAlgorithm,
      bagLocation = manifest.location.asInstanceOf[PrimaryS3StorageLocation],
      file = file,
      createdDate = manifest.createdDate
    )
  }
}
