package weco.storage_service.bagit.services

import java.io.InputStream

import org.scalatest.{EitherValues, TryValues}
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.bagit.models.BagPath
import weco.storage_service.checksum.ChecksumValue
import weco.storage.streaming.Codec._

class BagManifestParserTest
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with TryValues {
  it("parses a valid manifest") {
    val inputStream = toInputStream("""
      |abc123   file1.txt
      |def456   file2.txt
      |""".stripMargin)

    val manifest = BagManifestParser.parse(inputStream)

    manifest.success.value shouldBe Map(
      BagPath("file1.txt") -> ChecksumValue("abc123"),
      BagPath("file2.txt") -> ChecksumValue("def456")
    )
  }

  describe("catching errors in the manifest file") {
    it("spots duplicate entries") {
      val inputStream = toInputStream("""
                              |aaa111   file1.txt
                              |aaa111   file1.txt
                              |aaa222   file2.txt
                              |""".stripMargin)

      val result = BagManifestParser.parse(inputStream)

      val err = result.failure.exception

      err shouldBe a[RuntimeException]
      err.getMessage should startWith("Manifest contains duplicate paths:")
    }

    it("if a manifest has a line which isn't `checksum filepath`") {
      val inputStream = toInputStream("""
                              |notavalidline
                              |aaa111   file1.txt
                              |""".stripMargin)

      val result = BagManifestParser.parse(inputStream)

      val err = result.failure.exception

      err shouldBe a[RuntimeException]
      err.getMessage should startWith("Failed to parse the following lines:")
    }

    it("if the checksum is not hex-encoded") {
      val inputStream = toInputStream("""
                              |nothex   badfile.txt
                              |aaa111   file1.txt
                              |""".stripMargin)

      val result = BagManifestParser.parse(inputStream)

      val err = result.failure.exception

      err shouldBe a[RuntimeException]
      err.getMessage should startWith("Failed to parse the following lines:")
    }

    it("if the manifest does not contain any valid entries") {
      val inputStream = toInputStream("neroifuighsdfiug")

      val result = BagManifestParser.parse(inputStream)

      val err = result.failure.exception

      err shouldBe a[RuntimeException]
      err.getMessage should startWith("Failed to parse the following lines:")
    }
  }

  private def toInputStream(s: String): InputStream =
    stringCodec.toStream(s).value
}
