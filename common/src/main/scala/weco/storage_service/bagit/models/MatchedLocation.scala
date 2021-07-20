package weco.storage_service.bagit.models

import weco.storage_service.checksum.Checksum

case class MatchedLocation(
  bagPath: BagPath,
  checksum: Checksum,
  fetchMetadata: Option[BagFetchMetadata]
)
