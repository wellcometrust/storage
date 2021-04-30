package uk.ac.wellcome.platform.storage.bags.api.models

import io.circe.generic.extras.JsonKey
import uk.ac.wellcome.platform.archive.bag_tracker.models.BagVersionEntry
import uk.ac.wellcome.platform.archive.common.bagit.models.BagId
import uk.ac.wellcome.platform.archive.common.storage.models.StorageManifest

case class DisplayBagVersionEntry(
  id: String,
  version: String,
  createdDate: String,
  @JsonKey("type") ontologyType: String = "Bag"
)

case object DisplayBagVersionEntry {
  def apply(manifest: StorageManifest): DisplayBagVersionEntry =
    DisplayBagVersionEntry(
      id = manifest.id.toString,
      version = manifest.version.toString,
      createdDate = manifest.createdDate.toString
    )

  def apply(id: BagId, entry: BagVersionEntry): DisplayBagVersionEntry =
    DisplayBagVersionEntry(
      id = id.toString,
      version = entry.version.toString,
      createdDate = entry.createdDate.toString
    )
}