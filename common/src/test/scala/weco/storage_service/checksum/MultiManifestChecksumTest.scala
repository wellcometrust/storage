package weco.storage_service.checksum

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.generators.StorageRandomGenerators

import scala.util.Try

class MultiManifestChecksumTest
    extends AnyFunSpec
    with Matchers
    with StorageRandomGenerators {
  it("can be created") {
    MultiManifestChecksum(
      md5 = Some(randomChecksumValue),
      sha1 = Some(randomChecksumValue),
      sha256 = Some(randomChecksumValue),
      sha512 = Some(randomChecksumValue)
    )
  }

  it("can't be created if there are no values") {
    val result = Try {
      MultiManifestChecksum(
        md5 = None,
        sha1 = None,
        sha256 = None,
        sha512 = None
      )
    }

    result.failed.get shouldBe MultiManifestChecksumException.NoChecksums
  }

  it("can't be created if there are only weak values") {
    val result = Try {
      MultiManifestChecksum(
        md5 = Some(randomChecksumValue),
        sha1 = Some(randomChecksumValue),
        sha256 = None,
        sha512 = None
      )
    }

    result.failed.get shouldBe MultiManifestChecksumException.OnlyDeprecatedChecksums
  }
}
