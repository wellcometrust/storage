package uk.ac.wellcome.platform.archive.common.bagit.services

import java.net.URI

import org.scalatest.{FunSpec, Matchers}
import uk.ac.wellcome.platform.archive.common.bagit.models.{
  BagFetchMetadata,
  BagFile,
  BagPath
}
import uk.ac.wellcome.platform.archive.common.generators.{
  BagFileGenerators,
  BagGenerators,
  FetchMetadataGenerators
}
import uk.ac.wellcome.platform.archive.common.storage.Resolvable
import uk.ac.wellcome.platform.archive.common.verify.VerifiableLocation
import uk.ac.wellcome.storage.ObjectLocation
import uk.ac.wellcome.storage.generators.ObjectLocationGenerators

class BagVerifiableTest
    extends FunSpec
    with Matchers
    with BagGenerators
    with BagFileGenerators
    with FetchMetadataGenerators
    with ObjectLocationGenerators {
  implicit val resolvable: Resolvable[ObjectLocation] =
    (t: ObjectLocation) => new URI(s"example://${t.namespace}/${t.path}")

  val root: ObjectLocation = createObjectLocation
  val bagVerifiable = new BagVerifiable(root)

  describe("creates the correct list of VerifiableLocation") {
    it("for an empty bag") {
      val bag = createBag

      val bagVerifiable = new BagVerifiable(root)

      bagVerifiable.create(bag) shouldBe Right(List.empty)
    }

    it("for a bag that just has manifest files") {
      val manifestFiles = List(
        createBagFileWith("example.txt"),
        createBagFileWith("names.txt")
      )

      val bag = createBagWith(manifestFiles = manifestFiles)

      bagVerifiable.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(manifestFiles)
    }

    it("for a bag that just has tag manifest files") {
      val tagManifestFiles = List(
        createBagFileWith("tag-manifest-sha256.txt"),
        createBagFileWith("manifest-sha256.txt")
      )

      val bag = createBagWith(tagManifestFiles = tagManifestFiles)

      bagVerifiable.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(tagManifestFiles)
    }

    it("for a bag that has both file manifest and tag manifest files") {
      val manifestFiles = List(
        createBagFileWith("example.txt"),
        createBagFileWith("names.txt")
      )

      val tagManifestFiles = List(
        createBagFileWith("tag-manifest-sha256.txt"),
        createBagFileWith("manifest-sha256.txt")
      )

      val bag = createBagWith(
        manifestFiles = manifestFiles,
        tagManifestFiles = tagManifestFiles
      )

      bagVerifiable.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(manifestFiles ++ tagManifestFiles)
    }

    it("for a bag with fetch entries") {
      val manifestFiles = List(
        createBagFileWith("example.txt"),
        createBagFileWith("names.txt")
      )

      val fetchedManifestFiles = List(
        createBagFileWith("random.txt"),
        createBagFileWith("cat.jpg")
      )

      val fetchEntries = fetchedManifestFiles
        .map { _.path -> createFetchMetadata }
        .toMap

      val bag = createBagWith(
        manifestFiles = manifestFiles ++ fetchedManifestFiles,
        fetchEntries = fetchEntries
      )

      val expectedLocations =
        getExpectedLocations(manifestFiles) ++
          getExpectedLocations(fetchedManifestFiles, fetchEntries)

      bagVerifiable.create(bag).right.get should contain theSameElementsAs
        expectedLocations
    }

    it("for a bag with a repeated (but identical) fetch entry") {
      val manifestFiles = List(
        createBagFileWith("example.txt"),
        createBagFileWith("names.txt")
      )

      val fetchedManifestFiles = List(
        createBagFileWith("random.txt"),
        createBagFileWith("cat.jpg")
      )

      val fetchEntries = fetchedManifestFiles
        .map { _.path -> createFetchMetadata }
        .toMap

      val bag = createBagWith(
        manifestFiles = manifestFiles ++ fetchedManifestFiles,
        fetchEntries = fetchEntries ++ fetchEntries
      )

      val expectedLocations =
        getExpectedLocations(manifestFiles) ++
          getExpectedLocations(fetchedManifestFiles, fetchEntries)

      bagVerifiable.create(bag).right.get should contain theSameElementsAs
        expectedLocations
    }
  }

  describe("error cases") {
    it("there's a fetch entry for a file that isn't in the bag") {
      val fetchEntries = Map(
        BagPath("example.txt") -> createFetchMetadataWith(
          uri = "s3://example/example.txt"
        )
      )

      val bag = createBagWith(fetchEntries = fetchEntries)

      val result = bagVerifiable.create(bag)
      result shouldBe a[Left[_, _]]
      result.left.get.msg shouldBe "fetch.txt refers to paths that aren't in the bag manifest: example.txt"
    }

    it("there's are multiple fetch entries for a file that isn't in the bag") {
      val fetchEntries = Map(
        BagPath("example.txt") -> createFetchMetadataWith(
          uri = "s3://example/example.txt"
        ),
        BagPath("example.txt") -> createFetchMetadataWith(
          uri = "s3://example/red.txt"
        )
      )

      val bag = createBagWith(fetchEntries = fetchEntries)

      val result = bagVerifiable.create(bag)
      result shouldBe a[Left[_, _]]
      result.left.get.msg shouldBe
        "fetch.txt refers to paths that aren't in the bag manifest: example.txt"
    }
  }

  def createObjectLocationWith(root: ObjectLocation): ObjectLocation =
    root.join(randomAlphanumericWithLength(), randomAlphanumericWithLength())

  def getExpectedLocations(bagFiles: Seq[BagFile]): Seq[VerifiableLocation] =
    bagFiles.map { bagFile =>
      VerifiableLocation(
        path = bagFile.path,
        uri = new URI(
          s"example://${root.namespace}/${root.path}/${bagFile.path.toString}"
        ),
        checksum = bagFile.checksum,
        length = None
      )
    }

  def getExpectedLocations(
    bagFiles: Seq[BagFile],
    fetchEntries: Map[BagPath, BagFetchMetadata]
  ): Seq[VerifiableLocation] =
    bagFiles.map { bagFile =>
      val fetchMetadata = fetchEntries(bagFile.path)

      VerifiableLocation(
        uri = fetchMetadata.uri,
        path = bagFile.path,
        checksum = bagFile.checksum,
        length = fetchMetadata.length
      )
    }

}
