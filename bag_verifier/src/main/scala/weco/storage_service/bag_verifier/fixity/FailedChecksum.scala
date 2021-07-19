package weco.storage_service.bag_verifier.fixity

import weco.storage_service.bagit.models.MultiChecksumValue
import weco.storage_service.verify.{ChecksumValue, HashingResult}

sealed trait FailedChecksum

case class FailedChecksumCreation(e: Throwable)
    extends Throwable(s"Could not create checksum: ${e.getMessage}")
    with FailedChecksum

case class FailedChecksumNoMatch(
  actual: HashingResult,
  expected: MultiChecksumValue[ChecksumValue]
) extends Throwable(
      s"Checksum values do not match! Expected: $expected, saw: $actual"
    )
    with FailedChecksum

case class FailedChecksumLocationNotFound[T](expected: ExpectedFileFixity)
    extends Throwable("VerifiableLocation not found!")
    with FailedChecksum
