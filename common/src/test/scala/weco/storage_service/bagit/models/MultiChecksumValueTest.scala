package weco.storage_service.bagit.models

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.storage.models.FileManifest
import weco.storage_service.verify.{MD5, SHA1, SHA256, SHA512}

import scala.util.Try

class MultiChecksumValueTest extends AnyFunSpec with Matchers {
  it("can be created") {
    MultiChecksumValue(
      md5 = Some(FileManifest(MD5, files = Seq())),
      sha1 = Some(FileManifest(SHA1, files = Seq())),
      sha256 = Some(FileManifest(SHA256, files = Seq())),
      sha512 = Some(FileManifest(SHA512, files = Seq()))
    )
  }

  it("can't be created if there are no values") {
    val result = Try {
      MultiChecksumValue(
        md5 = None,
        sha1 = None,
        sha256 = None,
        sha512 = None
      )
    }

    result.failed.get shouldBe MultiChecksumException.NoChecksum
  }

  it("can't be created if there are only weak values") {
    val result = Try {
      MultiChecksumValue(
        md5 = Some(FileManifest(MD5, files = Seq())),
        sha1 = Some(FileManifest(MD5, files = Seq())),
        sha256 = None,
        sha512 = None
      )
    }

    result.failed.get shouldBe MultiChecksumException.OnlyWeakChecksums
  }
}
