package weco.storage_service.bag_verifier.fixity
import weco.storage_service.bagit.models.{BagPath, MultiChecksumValue}
import weco.storage_service.verify.ChecksumValue

import java.net.URI

sealed trait ExpectedFileFixity {
  val uri: URI
  val path: BagPath
  val multiChecksum: MultiChecksumValue[ChecksumValue]
  val length: Option[Long]
}

case class FetchFileFixity(
  uri: URI,
  path: BagPath,
  multiChecksum: MultiChecksumValue[ChecksumValue],
  length: Option[Long]
) extends ExpectedFileFixity

case class DataDirectoryFileFixity(
  uri: URI,
  path: BagPath,
  multiChecksum: MultiChecksumValue[ChecksumValue]
) extends ExpectedFileFixity {
  val length: Option[Long] = None
}
