package weco.storage_service.checksum

import java.io.{FilterInputStream, InputStream}

import org.scalatest.{EitherValues, TryValues}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage.streaming.Codec._

class MultiChecksumTest
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with TryValues {
  it("correctly hashes a string") {
    val input = "Hello world"
    val inputStream = stringCodec.toStream(input).value

    val result = MultiChecksum.create(inputStream)

    // Expected values obtained by searching "md5 Hello World" in DuckDuckGo,
    // and similar for other algorithms.
    result.success.value shouldBe MultiChecksum(
      md5 = ChecksumValue("3e25960a79dbc69b674cd4ec67a72c62"),
      sha1 = ChecksumValue("7b502c3a1f48c8609ae212cdfb639dee39673f5e"),
      sha256 = ChecksumValue(
        "64ec88ca00b268e5ba1a35678a1b5316d212f4f366b2477232534a8aeca37f3c"
      ),
      sha512 = ChecksumValue(
        "b7f783baed8297f0db917462184ff4f08e69c2d5e5f79a942600f9725f58ce1f29c18139bf80b06c0fff2bdd34738452ecf40c488c22a7e3d80cdf6f9c1c0d47"
      )
    )
  }

  // This test is ensuring we cross the STREAM_BUFFER_LENGTH boundary.
  it("hashes a string that is more than 1024 bytes long") {
    val input = "Hello world " * 1024
    val inputStream = stringCodec.toStream(input).value

    val result = MultiChecksum.create(inputStream)

    // Expected values obtained by using the Python hashlib library.
    result.success.value shouldBe MultiChecksum(
      md5 = ChecksumValue("6a2d892099d210d049be9276c2659c17"),
      sha1 = ChecksumValue("dc92fd07cfbfbb450eb0703fdea3ef9154e38db1"),
      sha256 = ChecksumValue(
        "54395c09aa8014ff92197da81f9d38d091d3674094c3c6890fafd0704670b34d"
      ),
      sha512 = ChecksumValue(
        "0516df5e826b287ef848c2362c72796475ced84d6bf41eeea920c0c8f053848f16352bed4ef951a7c8af1e7fb2b2525833dcbb538075fdafbb72f9c4627893f2"
      )
    )
  }

  it("returns a failure if it can't read the input stream") {
    val exception = new Throwable("BOOM!")

    class BrokenStream(is: InputStream) extends FilterInputStream(is) {
      override def read(b: Array[Byte], off: Int, len: Int): Int =
        throw exception
    }

    val inputStream = new BrokenStream(
      is = stringCodec.toStream("Hello world").value
    )

    val result = MultiChecksum.create(inputStream)
    result.failed.get shouldBe exception
  }

  describe("can be compared to a MultiManifestChecksum") {
    it("is a match if all the defined checksums are the same") {
      val expected = MultiManifestChecksum(
        md5 = Some(ChecksumValue("aaaaaaa")),
        sha256 = Some(ChecksumValue("ccccccc"))
      )

      val actual = MultiChecksum(
        md5 = ChecksumValue("aaaaaaa"),
        sha1 = ChecksumValue("bbbbbbb"),
        sha256 = ChecksumValue("ccccccc"),
        sha512 = ChecksumValue("ddddddd")
      )

      actual.matches(expected) shouldBe true
    }

    it("isn't a match if one of the checksums is difference") {
      val expected = MultiManifestChecksum(
        md5 = Some(ChecksumValue("aaaaaaa")),
        sha256 = Some(ChecksumValue("aaaaaaa"))
      )

      val actual = MultiChecksum(
        md5 = ChecksumValue("aaaaaab"),
        sha1 = ChecksumValue("bbbbbbb"),
        sha256 = ChecksumValue("ccccccc"),
        sha512 = ChecksumValue("ddddddd")
      )

      actual.matches(expected) shouldBe false
    }
  }
}
