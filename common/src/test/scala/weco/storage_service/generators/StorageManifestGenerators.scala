package weco.storage_service.generators

import java.time.Instant

import weco.storage_service.bagit.models.{BagInfo, BagVersion}
import weco.storage_service.ingests.models.IngestID
import weco.storage_service.storage.models._
import weco.storage_service.storage.services.DestinationBuilder
import weco.storage_service.checksum.{ChecksumAlgorithm, SHA256}

import scala.util.Random

trait StorageManifestGenerators
    extends BagInfoGenerators
    with StorageSpaceGenerators
    with ReplicaLocationGenerators {

  val checksumAlgorithm: ChecksumAlgorithm = SHA256

  def createStorageManifestFile: StorageManifestFile = {
    val path = createBagPath

    StorageManifestFile(
      checksum = randomChecksumValue,
      name = path.value,
      path = path.value,
      size = Random.nextLong().abs
    )
  }

  def createStorageManifestFileWith(
    pathPrefix: String = randomAlphanumeric(),
    name: String = randomAlphanumeric(),
    size: Long = Random.nextLong().abs
  ): StorageManifestFile = {

    val path = createBagPathWithPrefix(pathPrefix, name)

    StorageManifestFile(
      checksum = randomChecksumValue,
      name = name,
      path = path.value,
      size = size
    )
  }

  def createStorageManifestWith(
    ingestId: IngestID = createIngestID,
    space: StorageSpace = createStorageSpace,
    bagInfo: BagInfo = createBagInfo,
    version: BagVersion = createBagVersion,
    fileCount: Int = 3,
    location: PrimaryStorageLocation = PrimaryS3StorageLocation(
      createS3ObjectLocationPrefix
    ),
    replicaLocations: Seq[SecondaryStorageLocation] = (1 to randomInt(0, 5))
      .map { _ =>
        chooseFrom(
          SecondaryS3StorageLocation(createS3ObjectLocationPrefix),
          SecondaryAzureStorageLocation(createAzureBlobLocationPrefix)
        )
      },
    createdDate: Instant = Instant.now,
    files: Seq[StorageManifestFile] = Nil
  ): StorageManifest = {

    val pathPrefix = DestinationBuilder
      .buildPath(
        space,
        bagInfo.externalIdentifier,
        version
      )

    val fileManifestFiles = if (files.isEmpty) {
      (1 to fileCount)
        .map(_ => createStorageManifestFileWith(pathPrefix))
    } else {
      files
    }

    StorageManifest(
      space = space,
      info = bagInfo,
      version = version,
      manifest = FileManifest(
        checksumAlgorithm,
        files = fileManifestFiles
      ),
      tagManifest = FileManifest(
        checksumAlgorithm,
        files = Seq(
          createStorageManifestFileWith(pathPrefix),
          createStorageManifestFileWith(pathPrefix),
          createStorageManifestFileWith(pathPrefix)
        )
      ),
      location = location,
      replicaLocations = replicaLocations,
      createdDate = createdDate,
      ingestId = ingestId
    )
  }

  def createStorageManifestWithFileCount(fileCount: Int): StorageManifest =
    createStorageManifestWith(fileCount = fileCount)

  def createStorageManifest: StorageManifest =
    createStorageManifestWith()
}
