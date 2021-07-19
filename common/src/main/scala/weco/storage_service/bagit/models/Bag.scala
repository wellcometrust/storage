package weco.storage_service.bagit.models

case class Bag(
  info: BagInfo,
  manifest: NewPayloadManifest,
  tagManifest: NewTagManifest,
  fetch: Option[BagFetch] = None
)
