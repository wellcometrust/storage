package uk.ac.wellcome.platform.archive.bagreplicator.services

import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{FunSpec, Matchers}
import uk.ac.wellcome.json.JsonUtil._
import uk.ac.wellcome.platform.archive.bagreplicator.fixtures.{
  BagReplicatorFixtures,
  WorkerServiceFixture
}
import uk.ac.wellcome.platform.archive.common.models.bagit.{
  BagLocation,
  BagPath
}
import uk.ac.wellcome.platform.archive.common.models.{
  BagRequest,
  ReplicationResult
}
import uk.ac.wellcome.platform.archive.common.progress.ProgressUpdateAssertions
import uk.ac.wellcome.platform.archive.common.progress.models.Progress

class BagReplicatorWorkerServiceTest
    extends FunSpec
    with Matchers
    with ScalaFutures
    with BagReplicatorFixtures
    with ProgressUpdateAssertions
    with WorkerServiceFixture {

  it("replicates a bag successfully and updates both topics") {
    withLocalS3Bucket { bucket =>
      withLocalSnsTopic { progressTopic =>
        withLocalSnsTopic { outgoingTopic =>
          withWorkerService(
            progressTopic = progressTopic,
            outgoingTopic = outgoingTopic) { service =>
            withBag(bucket) { srcBagLocation =>
              val replicationRequest = BagRequest(
                requestId = randomUUID,
                bagLocation = srcBagLocation
              )

              val future = service.processMessage(replicationRequest)

              whenReady(future) { _ =>
                val outgoingMessages =
                  listMessagesReceivedFromSNS(outgoingTopic)
                val results =
                  outgoingMessages.map { msg =>
                    fromJson[ReplicationResult](msg.message).get
                  }.distinct

                results should have size 1
                val result = results.head
                result.archiveRequestId shouldBe replicationRequest.requestId
                result.srcBagLocation shouldBe replicationRequest.bagLocation

                val dstBagLocation = result.dstBagLocation

                verifyBagCopied(
                  src = srcBagLocation,
                  dst = dstBagLocation
                )

                assertTopicReceivesProgressEventUpdate(
                  replicationRequest.requestId,
                  progressTopic) { events =>
                  events should have size 1
                  events.head.description shouldBe "Bag replicated successfully"
                }
              }
            }
          }
        }
      }
    }
  }

  it("sends a failed ProgressUpdate if the bag fails to replicate") {
    withLocalSnsTopic { progressTopic =>
      withLocalSnsTopic { outgoingTopic =>
        withWorkerService(
          progressTopic = progressTopic,
          outgoingTopic = outgoingTopic) { service =>
          val srcBagLocation = BagLocation(
            storageNamespace = "does-not-exist",
            storagePrefix = Some("does/not/"),
            storageSpace = createStorageSpace,
            bagPath = BagPath("exist.txt")
          )

          val replicationRequest = BagRequest(
            requestId = randomUUID,
            bagLocation = srcBagLocation
          )

          val future = service.processMessage(replicationRequest)

          whenReady(future) { _ =>
            assertSnsReceivesNothing(outgoingTopic)

            assertTopicReceivesProgressStatusUpdate(
              replicationRequest.requestId,
              progressTopic = progressTopic,
              status = Progress.Failed) { events =>
              events should have size 1
              events.head.description shouldBe s"Failed to replicate bag"
            }
          }
        }
      }
    }
  }
}
