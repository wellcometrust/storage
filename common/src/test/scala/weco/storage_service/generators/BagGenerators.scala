package weco.storage_service.generators

import weco.storage_service.bagit.models._
import weco.storage_service.verify._

trait BagGenerators extends BagInfoGenerators {
  def createBagWith(
    manifestEntries: Map[BagPath, MultiChecksumValue[ChecksumValue]] = Map.empty,
    tagManifestEntries: Map[BagPath, MultiChecksumValue[ChecksumValue]] =
      Map.empty,
    fetchEntries: Map[BagPath, BagFetchMetadata] = Map.empty
  ): Bag =
    Bag(
      info = createBagInfo,
      manifest = NewPayloadManifest(entries = manifestEntries),
      tagManifest = NewTagManifest(entries = tagManifestEntries),
      fetch = if (fetchEntries.isEmpty) None else Some(BagFetch(fetchEntries))
    )

  def createBag: Bag = createBagWith()
}
