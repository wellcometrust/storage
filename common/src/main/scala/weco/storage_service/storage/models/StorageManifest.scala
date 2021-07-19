package weco.storage_service.storage.models

import java.time.Instant
import weco.storage_service.bagit.models.{BagId, BagInfo, BagVersion}
import weco.storage_service.ingests.models.IngestID
import weco.storage_service.verify.{ChecksumValue, HashingAlgorithm}

case class StorageManifestFile(
  checksum: ChecksumValue,
  name: String,
  path: String,
  size: Long
)

case class FileManifest(
  checksumAlgorithm: HashingAlgorithm,
  files: Seq[StorageManifestFile]
)

case class StorageManifest(
  space: StorageSpace,
  info: BagInfo,
  version: BagVersion,
  manifest: FileManifest,
  tagManifest: FileManifest,
  location: PrimaryStorageLocation,
  replicaLocations: Seq[SecondaryStorageLocation],
  createdDate: Instant,
  ingestId: IngestID
) {
  val id = BagId(space, info.externalIdentifier)
  val idWithVersion = s"$id/$version"
}
