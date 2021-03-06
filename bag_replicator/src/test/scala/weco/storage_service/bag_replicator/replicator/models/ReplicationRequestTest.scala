package weco.storage_service.bag_replicator.replicator.models

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.bag_replicator.models.{
  PrimaryReplica,
  SecondaryReplica
}
import weco.storage_service.storage.models.{
  PrimaryS3ReplicaLocation,
  SecondaryAzureReplicaLocation,
  SecondaryS3ReplicaLocation
}
import weco.storage.generators.{
  AzureBlobLocationGenerators,
  S3ObjectLocationGenerators
}

class ReplicationRequestTest
    extends AnyFunSpec
    with Matchers
    with S3ObjectLocationGenerators
    with AzureBlobLocationGenerators {
  describe("toReplicaLocation") {
    val s3Prefix = createS3ObjectLocationPrefix
    val azurePrefix = createAzureBlobLocationPrefix

    val s3Request = ReplicationRequest(
      srcPrefix = createS3ObjectLocationPrefix,
      dstPrefix = s3Prefix
    )

    val azureRequest = ReplicationRequest(
      srcPrefix = createS3ObjectLocationPrefix,
      dstPrefix = azurePrefix
    )

    it("a primary S3 replica") {
      s3Request.toReplicaLocation(replicaType = PrimaryReplica) shouldBe PrimaryS3ReplicaLocation(
        prefix = s3Prefix
      )
    }

    it("a secondary S3 replica") {
      s3Request.toReplicaLocation(replicaType = SecondaryReplica) shouldBe SecondaryS3ReplicaLocation(
        prefix = s3Prefix
      )
    }

    it("a secondary Azure replica") {
      azureRequest.toReplicaLocation(replicaType = SecondaryReplica) shouldBe SecondaryAzureReplicaLocation(
        prefix = azurePrefix
      )
    }

    it("does not allow a primary Azure replica") {
      intercept[IllegalArgumentException] {
        azureRequest.toReplicaLocation(replicaType = PrimaryReplica)
      }
    }
  }
}
