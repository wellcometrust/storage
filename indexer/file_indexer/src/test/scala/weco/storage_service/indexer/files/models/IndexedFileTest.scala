package weco.storage_service.indexer.files.models

import java.time.Instant

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.generators.StorageManifestGenerators
import weco.storage_service.storage.models.{
  PrimaryS3StorageLocation,
  StorageManifestFile
}
import weco.storage_service.checksum.{SHA256, SHA512}
import weco.storage_service.indexer.models.FileContext
import weco.storage.s3.{S3ObjectLocation, S3ObjectLocationPrefix}

class IndexedFileTest
    extends AnyFunSpec
    with Matchers
    with StorageManifestGenerators {
  it("creates an IndexedFile from a FileContext") {
    val context = FileContext(
      space = createStorageSpace,
      externalIdentifier = createExternalIdentifier,
      algorithm = chooseFrom(SHA256, SHA512),
      bagLocation = PrimaryS3StorageLocation(
        prefix = createS3ObjectLocationPrefix
      ),
      file = createStorageManifestFile,
      createdDate = Instant.now
    )

    val indexedFile = IndexedFile(context)

    indexedFile.space shouldBe context.space
    indexedFile.externalIdentifier shouldBe context.externalIdentifier
    indexedFile.name shouldBe context.file.name
    indexedFile.size shouldBe context.file.size
    indexedFile.location shouldBe context.bagLocation.prefix
      .asLocation(context.file.path)
    indexedFile.createdDate shouldBe context.createdDate

    indexedFile.checksum shouldBe IndexedChecksum(
      algorithm = context.algorithm.toString,
      value = context.file.checksum.toString
    )
  }

  it("uses the correct versioned path") {
    val file = StorageManifestFile(
      checksum = createChecksum.value,
      name = "data/cat.jpg",
      path = "v1/data/cat.jpg",
      size = 100
    )

    val context = FileContext(
      space = createStorageSpace,
      externalIdentifier = createExternalIdentifier,
      algorithm = chooseFrom(SHA256, SHA512),
      bagLocation = PrimaryS3StorageLocation(
        prefix = S3ObjectLocationPrefix(
          bucket = "example-storage",
          keyPrefix = "example/b1234"
        )
      ),
      file = file,
      createdDate = Instant.now
    )

    val indexedFile = IndexedFile(context)

    indexedFile.name shouldBe "data/cat.jpg"

    indexedFile.location shouldBe S3ObjectLocation(
      bucket = "example-storage",
      key = "example/b1234/v1/data/cat.jpg"
    )
  }

  it("uses the correct suffix") {
    val file = StorageManifestFile(
      checksum = createChecksum.value,
      name = "data/cat.jpg",
      path = "v1/data/cat.jpg",
      size = 100
    )

    val context = FileContext(
      space = createStorageSpace,
      externalIdentifier = createExternalIdentifier,
      algorithm = chooseFrom(SHA256, SHA512),
      bagLocation = PrimaryS3StorageLocation(
        createS3ObjectLocationPrefix
      ),
      file = file,
      createdDate = Instant.now
    )

    val indexedFile = IndexedFile(context)

    indexedFile.suffix shouldBe Some("jpg")
  }
}
