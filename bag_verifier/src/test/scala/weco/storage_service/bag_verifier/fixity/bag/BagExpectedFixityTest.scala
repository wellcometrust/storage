package weco.storage_service.bag_verifier.fixity.bag

import java.net.URI

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.bag_verifier.fixity.{
  DataDirectoryFileFixity,
  ExpectedFileFixity,
  FetchFileFixity
}
import weco.storage_service.bag_verifier.storage.Resolvable
import weco.storage_service.bagit.models.{BagFetchMetadata, BagPath}
import weco.storage_service.generators.{BagGenerators, FetchMetadataGenerators}
import weco.storage_service.checksum.{
  Checksum,
  ChecksumAlgorithm,
  ChecksumValue
}
import weco.storage.generators.MemoryLocationGenerators
import weco.storage.providers.memory.{MemoryLocation, MemoryLocationPrefix}

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
        BagPath("example.txt") -> randomChecksumValue,
        BagPath("names.txt") -> randomChecksumValue
      )

      val manifestChecksumAlgorithm = randomHashingAlgorithm

      val bag = createBagWith(
        manifestEntries = manifestEntries,
        manifestChecksumAlgorithm = manifestChecksumAlgorithm
      )

      bagExpectedFixity.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(
          manifestEntries = manifestEntries,
          checksumAlgorithm = manifestChecksumAlgorithm
        )
    }

    it("for a bag that just has tag manifest files") {
      val tagManifestEntries = Map(
        BagPath("tagmanifest-sha256.txt") -> randomChecksumValue,
        BagPath("manifest-sha256.txt") -> randomChecksumValue
      )

      val tagManifestChecksumAlgorithm = randomHashingAlgorithm

      val bag = createBagWith(
        tagManifestEntries = tagManifestEntries,
        tagManifestChecksumAlgorithm = tagManifestChecksumAlgorithm
      )

      bagExpectedFixity.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(
          manifestEntries = tagManifestEntries,
          checksumAlgorithm = tagManifestChecksumAlgorithm
        )
    }

    it("for a bag that has both file manifest and tag manifest files") {
      val manifestEntries = Map(
        BagPath("example.txt") -> randomChecksumValue,
        BagPath("names.txt") -> randomChecksumValue
      )

      val tagManifestEntries = Map(
        BagPath("tagmanifest-sha256.txt") -> randomChecksumValue,
        BagPath("manifest-sha256.txt") -> randomChecksumValue
      )

      val checksumAlgorithm = randomHashingAlgorithm

      val bag = createBagWith(
        manifestEntries = manifestEntries,
        manifestChecksumAlgorithm = checksumAlgorithm,
        tagManifestEntries = tagManifestEntries,
        tagManifestChecksumAlgorithm = checksumAlgorithm
      )

      bagExpectedFixity.create(bag).right.get should contain theSameElementsAs
        getExpectedLocations(
          manifestEntries = manifestEntries ++ tagManifestEntries,
          checksumAlgorithm = checksumAlgorithm
        )
    }

    it("for a bag with fetch entries") {
      val manifestEntries = Map(
        BagPath("example.txt") -> randomChecksumValue,
        BagPath("names.txt") -> randomChecksumValue
      )

      val fetchedManifestEntries = Map(
        BagPath("random.txt") -> randomChecksumValue,
        BagPath("cat.jpg") -> randomChecksumValue
      )

      val fetchEntries = fetchedManifestEntries.keys.map {
        _ -> createFetchMetadata
      }.toMap

      val manifestChecksumAlgorithm = randomHashingAlgorithm

      val bag = createBagWith(
        manifestEntries = manifestEntries ++ fetchedManifestEntries,
        manifestChecksumAlgorithm = manifestChecksumAlgorithm,
        fetchEntries = fetchEntries
      )

      val expectedLocations =
        getExpectedLocations(
          manifestEntries = manifestEntries,
          checksumAlgorithm = manifestChecksumAlgorithm
        ) ++
          getExpectedLocations(
            manifestEntries = fetchedManifestEntries,
            algorithm = manifestChecksumAlgorithm,
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
    manifestEntries: Map[BagPath, ChecksumValue],
    checksumAlgorithm: ChecksumAlgorithm
  ): Seq[ExpectedFileFixity] =
    manifestEntries.map {
      case (bagPath, checksumValue) =>
        DataDirectoryFileFixity(
          path = bagPath,
          uri = new URI(root.asLocation(bagPath.toString).toString),
          checksum = Checksum(
            algorithm = checksumAlgorithm,
            value = checksumValue
          )
        )
    }.toSeq

  def getExpectedLocations(
    manifestEntries: Map[BagPath, ChecksumValue],
    algorithm: ChecksumAlgorithm,
    fetchEntries: Map[BagPath, BagFetchMetadata]
  ): Seq[ExpectedFileFixity] =
    manifestEntries.map {
      case (bagPath, checksumValue) =>
        val fetchMetadata = fetchEntries(bagPath)

        FetchFileFixity(
          uri = fetchMetadata.uri,
          path = bagPath,
          checksum = Checksum(
            algorithm = algorithm,
            value = checksumValue
          ),
          length = fetchMetadata.length
        )
    }.toSeq
}
