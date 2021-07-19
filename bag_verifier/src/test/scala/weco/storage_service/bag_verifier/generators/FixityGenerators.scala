package weco.storage_service.bag_verifier.generators

import java.net.URI
import weco.storage_service.bag_verifier.fixity.{
  DataDirectoryFileFixity,
  ExpectedFileFixity,
  FetchFileFixity
}
import weco.storage_service.generators.StorageRandomGenerators
import weco.storage_service.verify.{Checksum, ChecksumValue, MD5, SHA256}
import weco.storage.Location
import weco.storage_service.bagit.models.MultiChecksumValue

trait FixityGenerators[BagLocation <: Location]
    extends StorageRandomGenerators {

  def randomChecksum = Checksum(SHA256, randomChecksumValue)
  def badChecksum = Checksum(MD5, randomChecksumValue)

  def resolve(location: BagLocation): URI

  def createExpectedFileFixity: ExpectedFileFixity =
    createDataDirectoryFileFixityWith()

  def createLocation: BagLocation

  def createFetchFileFixityWith(
    location: BagLocation = createLocation,
    multiChecksum: MultiChecksumValue[ChecksumValue] = randomMultiChecksum,
    length: Option[Long] = None
  ): FetchFileFixity =
    FetchFileFixity(
      uri = resolve(location),
      path = createBagPath,
      multiChecksum = multiChecksum,
      length = length
    )

  def createDataDirectoryFileFixityWith(
    location: BagLocation = createLocation,
    multiChecksum: MultiChecksumValue[ChecksumValue] = randomMultiChecksum
  ): DataDirectoryFileFixity =
    DataDirectoryFileFixity(
      uri = resolve(location),
      path = createBagPath,
      multiChecksum = multiChecksum
    )

  def createDataDirectoryFileFixity: DataDirectoryFileFixity =
    createDataDirectoryFileFixityWith()
}
