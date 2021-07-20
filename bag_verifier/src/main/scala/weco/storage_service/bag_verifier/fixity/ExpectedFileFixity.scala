package weco.storage_service.bag_verifier.fixity
import java.net.URI

import weco.storage_service.bagit.models.BagPath
import weco.storage_service.checksum.Checksum

sealed trait ExpectedFileFixity {
  val uri: URI
  val path: BagPath
  val checksum: Checksum
  val length: Option[Long]
}

case class FetchFileFixity(
  uri: URI,
  path: BagPath,
  checksum: Checksum,
  length: Option[Long]
) extends ExpectedFileFixity

case class DataDirectoryFileFixity(
  uri: URI,
  path: BagPath,
  checksum: Checksum
) extends ExpectedFileFixity {
  val length: Option[Long] = None
}
