package uk.ac.wellcome.platform.archive.common.generators

import uk.ac.wellcome.platform.archive.common.fixtures.StorageRandomThings
import uk.ac.wellcome.platform.archive.common.storage.models._
import uk.ac.wellcome.storage.generators.{
  AzureBlobLocationGenerators,
  S3ObjectLocationGenerators
}

trait ReplicaLocationGenerators extends StorageRandomThings with S3ObjectLocationGenerators with AzureBlobLocationGenerators {
  def createPrimaryLocation: PrimaryReplicaLocation =
    chooseFrom(
      Seq(
        PrimaryS3ReplicaLocation(prefix = createS3ObjectLocationPrefix)
      )
    )

  def createSecondaryLocation: SecondaryReplicaLocation =
    chooseFrom(
      Seq(
        SecondaryS3ReplicaLocation(prefix = createS3ObjectLocationPrefix),
        SecondaryAzureReplicaLocation(prefix = createAzureBlobLocationPrefix)
      )
    )
}
