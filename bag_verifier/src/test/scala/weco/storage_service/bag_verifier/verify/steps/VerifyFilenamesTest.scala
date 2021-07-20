package weco.storage_service.bag_verifier.verify.steps

import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.bagit.models.{BagPath, PayloadManifest, TagManifest}
import weco.storage_service.checksum.{ChecksumValue, MD5}

class VerifyFilenamesTest extends AnyFunSpec with Matchers with EitherValues {
  val verifier: VerifyFilenames =
    new VerifyFilenames {}

  describe("verifyAllowedFilenames") {
    it("allows legal filenames") {
      verifier.verifyAllowedFilenames(Seq("cat.jpg", "dog.png", "fish.gif")) shouldBe Right(
        ()
      )
    }

    it("flags a file with a trailing dot") {
      val err = verifier.verifyAllowedFilenames(Seq("bad.jpg.")).left.value
      err.e.getMessage shouldBe "Filenames cannot end with a .: bad.jpg."
      err.userMessage shouldBe Some("Filenames cannot end with a .: bad.jpg.")
    }

    it("flags multiple files with a trailing dot") {
      val err = verifier
        .verifyAllowedFilenames(Seq("bad.jpg.", "alsobad.png.", "good.tif"))
        .left
        .value
      err.e.getMessage shouldBe "Filenames cannot end with a .: bad.jpg., alsobad.png."
      err.userMessage shouldBe Some(
        "Filenames cannot end with a .: bad.jpg., alsobad.png."
      )
    }
  }

  describe("verifyPayloadFilenames") {
    it("allows filenames that start with data/") {
      val manifest = PayloadManifest(
        checksumAlgorithm = MD5,
        entries = Map(
          BagPath("data/README.txt") -> ChecksumValue("123"),
          BagPath("data/animals/cat.jpg") -> ChecksumValue("123"),
          BagPath("data/animals/dog.png") -> ChecksumValue("123")
        )
      )

      verifier.verifyPayloadFilenames(manifest) shouldBe Right(())
    }

    it("allows an empty manifest") {
      val manifest = PayloadManifest(
        checksumAlgorithm = MD5,
        entries = Map.empty
      )

      verifier.verifyPayloadFilenames(manifest) shouldBe Right(())
    }

    it("fails a manifest with filenames outside data/") {
      val manifest = PayloadManifest(
        checksumAlgorithm = MD5,
        entries = Map(
          BagPath("data/README.txt") -> ChecksumValue("123"),
          BagPath("data/animals/cat.jpg") -> ChecksumValue("123"),
          BagPath("dog.png") -> ChecksumValue("123"),
          BagPath("tags/metadata.csv") -> ChecksumValue("123")
        )
      )

      val err = verifier.verifyPayloadFilenames(manifest).left.value
      err.e.getMessage shouldBe "Not all payload files are in the data/ directory: dog.png, tags/metadata.csv"
      err.userMessage shouldBe Some(
        "Not all payload files are in the data/ directory: dog.png, tags/metadata.csv"
      )
    }
  }

  describe("verifyTagFileFilenames") {
    it("allows tag files in the root") {
      val manifest = TagManifest(
        checksumAlgorithm = MD5,
        entries = Map(
          BagPath("bagit.txt") -> ChecksumValue("123"),
          BagPath("bag-info.txt") -> ChecksumValue("123")
        )
      )

      verifier.verifyTagFileFilenames(manifest) shouldBe Right(())
    }

    it("fails a manifest with tag files in subdirectories") {
      val manifest = TagManifest(
        checksumAlgorithm = MD5,
        entries = Map(
          BagPath("bagit.txt") -> ChecksumValue("123"),
          BagPath("data/bag-info.txt") -> ChecksumValue("123"),
          BagPath("tags/metadata.csv") -> ChecksumValue("123")
        )
      )

      val err = verifier.verifyTagFileFilenames(manifest).left.value
      err.e.getMessage shouldBe "Not all tag files are in the root directory: data/bag-info.txt, tags/metadata.csv"
      err.userMessage shouldBe Some(
        "Not all tag files are in the root directory: data/bag-info.txt, tags/metadata.csv"
      )
    }
  }
}
