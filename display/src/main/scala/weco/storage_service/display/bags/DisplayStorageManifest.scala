package weco.storage_service.display.bags

import java.net.URL

import io.circe.generic.extras.JsonKey
import weco.storage_service.storage.models.StorageManifest
import weco.storage_service.display.{DisplayLocation, DisplayStorageSpace}

case class DisplayStorageManifest(
  @JsonKey("@context") context: String,
  id: String,
  space: DisplayStorageSpace,
  info: ResponseDisplayBagInfo,
  manifest: DisplayFileManifest,
  tagManifest: DisplayFileManifest,
  location: DisplayLocation,
  replicaLocations: Seq[DisplayLocation],
  createdDate: String,
  version: String,
  @JsonKey("type") ontologyType: String = "Bag"
)

object DisplayStorageManifest {
  def apply(
    storageManifest: StorageManifest,
    contextUrl: URL
  ): DisplayStorageManifest =
    DisplayStorageManifest(
      context = contextUrl.toString,
      id = storageManifest.id.toString,
      space = DisplayStorageSpace(storageManifest.space.underlying),
      info = ResponseDisplayBagInfo(storageManifest.info),
      manifest = DisplayFileManifest(storageManifest.manifest),
      tagManifest = DisplayFileManifest(storageManifest.tagManifest),
      location = DisplayLocation(storageManifest.location),
      replicaLocations = storageManifest.replicaLocations.map {
        DisplayLocation(_)
      },
      createdDate = storageManifest.createdDate.toString,
      version = storageManifest.version.toString
    )
}
