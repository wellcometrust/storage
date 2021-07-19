package weco.storage_service.bag_verifier.fixity.bag

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage.generators.MemoryLocationGenerators
import weco.storage.providers.memory.{MemoryLocation, MemoryLocationPrefix}
import weco.storage_service.bag_verifier.fixity.{
  DataDirectoryFileFixity,
  ExpectedFileFixity,
  FetchFileFixity
}
import weco.storage_service.bag_verifier.storage.Resolvable
import weco.storage_service.bagit.models.{
  BagFetchMetadata,
  BagPath,
  MultiChecksumValue
}
import weco.storage_service.generators.{BagGenerators, FetchMetadataGenerators}
import weco.storage_service.verify.ChecksumValue

import java.net.URI

class BagExpectedFixityTest
    extends AnyFunSpec
    with Matchers
    with BagGenerators
    with FetchMetadataGenerators
    with MemoryLocationGenerators {
  implicit val resolvable: Resolvable[MemoryLocation] =
    (location: MemoryLocation) => new URI(location.toString)

  val root: MemoryLocationPrefix = createMemoryLocationPrefix

  val bagExpectedFixity =
    new BagExpectedFixity[MemoryLocation, MemoryLocationPrefix](root)

  def createLocationWith(root: MemoryLocation): MemoryLocation =
    MemoryLocation(
      namespace = root.namespace,
      path = s"${root.path}/${randomAlphanumeric()}/${randomAlphanumeric()}"
    )

  describe("creates the correct list of VerifiableLocation") {
    it("for an empty bag") {
      val bag = createBag

      val bagExpectedFixity =
        new BagExpectedFixity[MemoryLocation, MemoryLocationPrefix](root)

      bagExpectedFixity.create(bag) shouldBe Right(List.empty)
    }

    it("for a bag that just has manifest files") {
      val manifestEntries = Map(
        BagPath("example.txt") -> randomMultiChecksum,
        BagPath("names.txt") -> randomMultiChecksum
      )

      val bag = createBagWith(
        manifestEntries = manifestEntries
      )

      bagExpectedFixity.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(manifestEntries = manifestEntries)
    }

    it("for a bag that just has tag manifest files") {
      val tagManifestEntries = Map(
        BagPath("tagmanifest-sha256.txt") -> randomMultiChecksum,
        BagPath("manifest-sha256.txt") -> randomMultiChecksum
      )

      val bag = createBagWith(tagManifestEntries = tagManifestEntries)

      bagExpectedFixity.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(manifestEntries = tagManifestEntries)
    }

    it("for a bag that has both file manifest and tag manifest files") {
      val manifestEntries = Map(
        BagPath("example.txt") -> randomMultiChecksum,
        BagPath("names.txt") -> randomMultiChecksum
      )

      val tagManifestEntries = Map(
        BagPath("tagmanifest-sha256.txt") -> randomMultiChecksum,
        BagPath("manifest-sha256.txt") -> randomMultiChecksum
      )

      val bag = createBagWith(
        manifestEntries = manifestEntries,
        tagManifestEntries = tagManifestEntries
      )

      bagExpectedFixity.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(
          manifestEntries = manifestEntries ++ tagManifestEntries
        )
    }

    it("for a bag with fetch entries") {
      val manifestEntries = Map(
        BagPath("example.txt") -> randomMultiChecksum,
        BagPath("names.txt") -> randomMultiChecksum
      )

      val fetchedManifestEntries = Map(
        BagPath("random.txt") -> randomMultiChecksum,
        BagPath("cat.jpg") -> randomMultiChecksum
      )

      val fetchEntries = fetchedManifestEntries.keys.map {
        _ -> createFetchMetadata
      }.toMap

      val bag = createBagWith(
        manifestEntries = manifestEntries ++ fetchedManifestEntries,
        fetchEntries = fetchEntries
      )

      val expectedLocations =
        getExpectedLocations(
          manifestEntries = manifestEntries
        ) ++
          getExpectedLocations(
            manifestEntries = fetchedManifestEntries,
            fetchEntries = fetchEntries
          )

      bagExpectedFixity.create(bag).right.get should contain theSameElementsAs
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

      val result = bagExpectedFixity.create(bag)
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

      val result = bagExpectedFixity.create(bag)
      result shouldBe a[Left[_, _]]
      result.left.get.msg shouldBe
        "fetch.txt refers to paths that aren't in the bag manifest: example.txt"
    }
  }

  def getExpectedLocations(
    manifestEntries: Map[BagPath, MultiChecksumValue[ChecksumValue]]
  ): Seq[ExpectedFileFixity] =
    manifestEntries.map {
      case (bagPath, multiChecksum) =>
        DataDirectoryFileFixity(
          path = bagPath,
          uri = new URI(root.asLocation(bagPath.toString).toString),
          multiChecksum = multiChecksum
        )
    }.toSeq

  def getExpectedLocations(
    manifestEntries: Map[BagPath, MultiChecksumValue[ChecksumValue]],
    fetchEntries: Map[BagPath, BagFetchMetadata]
  ): Seq[ExpectedFileFixity] =
    manifestEntries.map {
      case (bagPath, multiChecksum) =>
        val fetchMetadata = fetchEntries(bagPath)

        FetchFileFixity(
          uri = fetchMetadata.uri,
          path = bagPath,
          multiChecksum = multiChecksum,
          length = fetchMetadata.length
        )
    }.toSeq
}
