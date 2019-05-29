package uk.ac.wellcome.platform.archive.bag_register.fixtures

import org.scalatest.Assertion
import uk.ac.wellcome.fixtures.TestWith
import uk.ac.wellcome.messaging.fixtures.SQS.QueuePair
import uk.ac.wellcome.messaging.fixtures.worker.AlpakkaSQSWorkerFixtures
import uk.ac.wellcome.messaging.memory.MemoryMessageSender
import uk.ac.wellcome.platform.archive.bag_register.services.{
  BagRegisterWorker,
  Register
}
import uk.ac.wellcome.platform.archive.common.IngestID
import uk.ac.wellcome.platform.archive.common.bagit.models.BagId
import uk.ac.wellcome.platform.archive.common.fixtures.{
  MonitoringClientFixture,
  OperationFixtures,
  RandomThings,
  StorageManifestVHSFixture
}
import uk.ac.wellcome.platform.archive.common.ingests.fixtures.IngestUpdateAssertions
import uk.ac.wellcome.platform.archive.common.ingests.models.{
  Ingest,
  IngestStatusUpdate
}
import uk.ac.wellcome.platform.archive.common.storage.services.StorageManifestService

trait BagRegisterFixtures
    extends RandomThings
    with AlpakkaSQSWorkerFixtures
    with OperationFixtures
    with StorageManifestVHSFixture
    with MonitoringClientFixture
    with IngestUpdateAssertions {

  type Fixtures = (BagRegisterWorker[String, String],
                   StorageManifestDao,
                   StorageManifestStore,
                   MemoryMessageSender,
                   MemoryMessageSender,
                   QueuePair)

  def withBagRegisterWorker[R](testWith: TestWith[Fixtures, R]): R =
    withActorSystem { implicit actorSystem =>
      withMonitoringClient { implicit monitoringClient =>
        val dao = createDao
        val store = createStore
        val storageManifestVHS = createStorageManifestVHS(dao, store)

        val ingests = new MemoryMessageSender()
        val outgoing = new MemoryMessageSender()

        withLocalSqsQueueAndDlq { queuePair =>
          val storageManifestService = new StorageManifestService()

          val register = new Register(
            storageManifestService,
            storageManifestVHS
          )

          val service = new BagRegisterWorker(
            alpakkaSQSWorkerConfig =
              createAlpakkaSQSWorkerConfig(queuePair.queue),
            ingestUpdater =
              createIngestUpdaterWith(ingests, stepName = "register"),
            outgoingPublisher = createOutgoingPublisherWith(outgoing),
            register = register
          )

          service.run()

          testWith(
            (service, dao, store, ingests, outgoing, queuePair)
          )
        }
      }
    }

  def assertBagRegisterSucceeded(ingestId: IngestID,
                                 ingests: MemoryMessageSender,
                                 bagId: BagId): Assertion =
    assertTopicReceivesIngestUpdates(ingestId, ingests) { ingestUpdates =>
      ingestUpdates.size shouldBe 2

      val ingestStart = ingestUpdates.head
      ingestStart.events.head.description shouldBe "Register started"

      val ingestCompleted =
        ingestUpdates.tail.head.asInstanceOf[IngestStatusUpdate]
      ingestCompleted.status shouldBe Ingest.Completed
      ingestCompleted.affectedBag shouldBe Some(bagId)
      ingestCompleted.events.head.description shouldBe "Register succeeded (completed)"
    }

  def assertBagRegisterFailed(ingestId: IngestID,
                              ingests: MemoryMessageSender,
                              bagId: Option[BagId] = None): Assertion =
    assertTopicReceivesIngestUpdates(ingestId, ingests) { ingestUpdates =>
      ingestUpdates.size shouldBe 2

      val ingestStart = ingestUpdates.head
      ingestStart.events.head.description shouldBe "Register started"

      val ingestFailed =
        ingestUpdates.tail.head.asInstanceOf[IngestStatusUpdate]
      ingestFailed.status shouldBe Ingest.Failed
      ingestFailed.affectedBag shouldBe bagId
      ingestFailed.events.head.description shouldBe "Register failed"
    }
}
