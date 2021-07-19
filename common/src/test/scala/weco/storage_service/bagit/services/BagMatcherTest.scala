package weco.storage_service.bagit.services

import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.bagit.models.{
  MatchedLocation,
  MultiChecksumValue,
  NewPayloadManifest
}
import weco.storage_service.generators.{
  FetchMetadataGenerators,
  StorageRandomGenerators
}
import weco.storage_service.verify.ChecksumValue

class BagMatcherTest
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with FetchMetadataGenerators
    with StorageRandomGenerators {

  def randomMultiChecksumValue: MultiChecksumValue[ChecksumValue] =
    MultiChecksumValue(sha256 = Some(randomChecksumValue))

  describe("creates the correct list of MatchedLocations") {
    it("for an empty bag") {
      BagMatcher
        .correlateFetchEntryToBagFile(
          manifest = NewPayloadManifest(entries = Map()),
          fetchEntries = Map.empty
        )
        .value shouldBe Seq.empty
    }

    it("for a bag that doesn't have any fetch entries") {
      val manifestEntries = Map(
        createBagPath -> randomMultiChecksumValue,
        createBagPath -> randomMultiChecksumValue,
        createBagPath -> randomMultiChecksumValue
      )

      val result = BagMatcher.correlateFetchEntryToBagFile(
        manifest = NewPayloadManifest(entries = manifestEntries),
        fetchEntries = Map.empty
      )

      result.value shouldBe manifestEntries.map {
        case (bagPath, checksum) =>
          MatchedLocation(
            bagPath = bagPath,
            checksum = checksum,
            fetchMetadata = None
          )
      }
    }

    it("for a bag with fetch entries") {
      val manifestEntries = Map(
        createBagPath -> randomMultiChecksumValue,
        createBagPath -> randomMultiChecksumValue,
        createBagPath -> randomMultiChecksumValue
      )

      val fetchMetadata = createFetchMetadata
      val fetchPath = createBagPath
      val fetchChecksumValue = randomMultiChecksumValue

      val result = BagMatcher.correlateFetchEntryToBagFile(
        manifest = NewPayloadManifest(
          entries = manifestEntries ++ Map(fetchPath -> fetchChecksumValue)
        ),
        fetchEntries = Map(fetchPath -> fetchMetadata)
      )

      val expectedLocations = manifestEntries.map {
        case (bagPath, checksum) =>
          MatchedLocation(
            bagPath = bagPath,
            checksum = checksum,
            fetchMetadata = None
          )
      }.toSeq :+ MatchedLocation(
        bagPath = fetchPath,
        checksum = fetchChecksumValue,
        fetchMetadata = Some(fetchMetadata)
      )

      result.value should contain theSameElementsAs expectedLocations
    }
  }

  describe("error cases") {
    it("there's a fetch entry for a file that isn't in the bag") {
      val fetchPath = createBagPath
      val fetchEntries = Map(fetchPath -> createFetchMetadata)

      val result = BagMatcher.correlateFetchEntryToBagFile(
        manifest = NewPayloadManifest(entries = Map()),
        fetchEntries = fetchEntries
      )

      result.left.value.getMessage shouldBe
        s"fetch.txt refers to paths that aren't in the bag manifest: $fetchPath"
    }

    it("there are multiple fetch entries for files that aren't in the bag") {
      val fetchEntries = (1 to 3).map { _ =>
        createBagPath -> createFetchMetadata
      }.toMap

      val result = BagMatcher.correlateFetchEntryToBagFile(
        manifest = NewPayloadManifest(entries = Map()),
        fetchEntries = fetchEntries
      )

      result.left.value.getMessage should startWith(
        s"fetch.txt refers to paths that aren't in the bag manifest:"
      )
    }
  }
}
