package weco.storage_service.bag_verifier.generators

import java.net.URI

import weco.storage_service.bag_verifier.fixity.{
  DataDirectoryFileFixity,
  ExpectedFileFixity,
  FetchFileFixity
}
import weco.storage_service.generators.StorageRandomGenerators
import weco.storage_service.checksum.{Checksum, MD5, SHA256}
import weco.storage.Location

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
    checksum: Checksum = randomChecksum,
    length: Option[Long] = None
  ): FetchFileFixity =
    FetchFileFixity(
      uri = resolve(location),
      path = createBagPath,
      checksum = checksum,
      length = length
    )

  def createDataDirectoryFileFixityWith(
    location: BagLocation = createLocation,
    checksum: Checksum = randomChecksum
  ): DataDirectoryFileFixity =
    DataDirectoryFileFixity(
      uri = resolve(location),
      path = createBagPath,
      checksum = checksum
    )

  def createDataDirectoryFileFixity: DataDirectoryFileFixity =
    createDataDirectoryFileFixityWith()
}
