package weco.storage_service.bag_register.services

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{EitherValues, TryValues}
import weco.fixtures.{RandomGenerators, TestWith}
import weco.storage_service.storage.models.StorageManifestFile
import weco.storage_service.checksum.{ChecksumValue, MD5, SHA256}
import weco.storage.StoreReadError
import weco.storage.providers.memory.{MemoryLocation, MemoryLocationPrefix}
import weco.storage.store.Readable
import weco.storage.store.memory.{MemoryStore, MemoryStreamStore}
import weco.storage.streaming.InputStreamWithLength

class TagManifestFileFinderTest
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with TryValues
    with RandomGenerators {

  def withTagManifestFileFinder[R](
    entries: Map[MemoryLocation, String]
  )(testWith: TestWith[TagManifestFileFinder[MemoryLocation], R]): R = {
    val memoryStore: MemoryStore[MemoryLocation, Array[Byte]] =
      new MemoryStore[MemoryLocation, Array[Byte]](initialEntries = Map.empty)

    entries
      .foreach {
        case (location, str) =>
          memoryStore.put(location)(str.getBytes())
      }

    implicit val streamStore: MemoryStreamStore[MemoryLocation] =
      new MemoryStreamStore[MemoryLocation](memoryStore)

    testWith(new TagManifestFileFinder())
  }

  it("handles a bag that contains all four tag manifest files") {
    val prefix = MemoryLocationPrefix(
      namespace = randomAlphanumeric(),
      path = randomAlphanumeric()
    )

    val result =
      withTagManifestFileFinder(
        entries = Map(
          prefix.asLocation("tagmanifest-md5.txt") -> "My MD5 tag manifest",
          prefix.asLocation("tagmanifest-sha1.txt") -> "My SHA1 tag manifest",
          prefix
            .asLocation("tagmanifest-sha256.txt") -> "My SHA256 tag manifest",
          prefix
            .asLocation("tagmanifest-sha512.txt") -> "My SHA512 tag manifest"
        )
      ) {
        _.getTagManifestFiles(prefix, algorithm = SHA256)
      }

    result.success.value should contain theSameElementsAs Seq(
      StorageManifestFile(
        checksum = ChecksumValue(
          "fe9a209b4ef3426c9b30e7808047689e3bacedb0aa58db91cf2aa355834199c1"
        ),
        name = "tagmanifest-md5.txt",
        size = 19,
        path = "tagmanifest-md5.txt"
      ),
      StorageManifestFile(
        checksum = ChecksumValue(
          "3a2d9e87f82ca9c6fb16b8af5968e405f8952cedc73134e3b222a6e5f45aedff"
        ),
        name = "tagmanifest-sha1.txt",
        size = 20,
        path = "tagmanifest-sha1.txt"
      ),
      StorageManifestFile(
        checksum = ChecksumValue(
          "1359a6582aa673ed377f017abd62f2f4f6fc77cb9477423444b95ba0996f3f14"
        ),
        name = "tagmanifest-sha256.txt",
        size = 22,
        path = "tagmanifest-sha256.txt"
      ),
      StorageManifestFile(
        checksum = ChecksumValue(
          "e749dc748023730bee5b04f477934cc7fc8e4dc4d98521a82aef471c38235fd6"
        ),
        name = "tagmanifest-sha512.txt",
        size = 22,
        path = "tagmanifest-sha512.txt"
      )
    )
  }

  it("uses the selected algorithm to create the checksums") {
    val prefix = MemoryLocationPrefix(
      namespace = randomAlphanumeric(),
      path = randomAlphanumeric()
    )

    val result =
      withTagManifestFileFinder(
        entries = Map(
          prefix.asLocation("tagmanifest-md5.txt") -> "My MD5 tag manifest",
          prefix.asLocation("tagmanifest-sha1.txt") -> "My SHA1 tag manifest",
          prefix
            .asLocation("tagmanifest-sha256.txt") -> "My SHA256 tag manifest",
          prefix
            .asLocation("tagmanifest-sha512.txt") -> "My SHA512 tag manifest"
        )
      ) {
        _.getTagManifestFiles(prefix, algorithm = MD5)
      }

    result.success.value should contain theSameElementsAs Seq(
      StorageManifestFile(
        checksum = ChecksumValue("825a10d25c6d52e83482918cbe9320ac"),
        name = "tagmanifest-md5.txt",
        size = 19,
        path = "tagmanifest-md5.txt"
      ),
      StorageManifestFile(
        checksum = ChecksumValue("e32d1253cebc7426cf09f5ff18153333"),
        name = "tagmanifest-sha1.txt",
        size = 20,
        path = "tagmanifest-sha1.txt"
      ),
      StorageManifestFile(
        checksum = ChecksumValue("0c55b168d2be86bcf9e372cd0128154c"),
        name = "tagmanifest-sha256.txt",
        size = 22,
        path = "tagmanifest-sha256.txt"
      ),
      StorageManifestFile(
        checksum = ChecksumValue("2c7c12559437326973adc7ec5a0328ab"),
        name = "tagmanifest-sha512.txt",
        size = 22,
        path = "tagmanifest-sha512.txt"
      )
    )
  }

  it("returns only the tag manifest files that are present") {
    val prefix = MemoryLocationPrefix(
      namespace = randomAlphanumeric(),
      path = randomAlphanumeric()
    )

    val result =
      withTagManifestFileFinder(
        entries = Map(
          prefix
            .asLocation("tagmanifest-sha256.txt") -> "My SHA256 tag manifest",
          prefix
            .asLocation("tagmanifest-sha512.txt") -> "My SHA512 tag manifest"
        )
      ) {
        _.getTagManifestFiles(prefix, algorithm = SHA256)
      }

    result.success.value should contain theSameElementsAs Seq(
      StorageManifestFile(
        checksum = ChecksumValue(
          "1359a6582aa673ed377f017abd62f2f4f6fc77cb9477423444b95ba0996f3f14"
        ),
        name = "tagmanifest-sha256.txt",
        size = 22,
        path = "tagmanifest-sha256.txt"
      ),
      StorageManifestFile(
        checksum = ChecksumValue(
          "e749dc748023730bee5b04f477934cc7fc8e4dc4d98521a82aef471c38235fd6"
        ),
        name = "tagmanifest-sha512.txt",
        size = 22,
        path = "tagmanifest-sha512.txt"
      )
    )
  }

  it("fails if it doesn't find any files") {
    val prefix = MemoryLocationPrefix(
      namespace = randomAlphanumeric(),
      path = randomAlphanumeric()
    )

    val result = withTagManifestFileFinder(entries = Map.empty) {
      _.getTagManifestFiles(prefix, algorithm = SHA256)
    }

    result.failed.get shouldBe a[RuntimeException]
    result.failed.get.getMessage shouldBe s"No tag manifest files found under $prefix"
  }

  it("fails if the underlying reader has an error") {
    val prefix = MemoryLocationPrefix(
      namespace = randomAlphanumeric(),
      path = randomAlphanumeric()
    )

    implicit val brokenReader =
      new Readable[MemoryLocation, InputStreamWithLength] {
        override def get(id: MemoryLocation): this.ReadEither =
          Left(StoreReadError(new Throwable("BOOM!")))
      }

    val tagManifestFileFinder = new TagManifestFileFinder()

    val result = tagManifestFileFinder.getTagManifestFiles(
      prefix = prefix,
      algorithm = SHA256
    )

    result.failed.get shouldBe a[RuntimeException]
    result.failed.get.getMessage should startWith(
      s"Error looking up $prefix/tagmanifest-"
    )
  }
}
