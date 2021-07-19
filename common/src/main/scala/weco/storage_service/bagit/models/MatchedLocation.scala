package weco.storage_service.bagit.models

import weco.storage_service.verify.ChecksumValue

case class MatchedLocation(
  bagPath: BagPath,
  checksum: MultiChecksumValue[ChecksumValue],
  fetchMetadata: Option[BagFetchMetadata]
)
