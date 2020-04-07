package uk.ac.wellcome.platform.archive.ingests.services

import io.circe.Encoder
import org.scalatest.{FunSpec, TryValues}
import uk.ac.wellcome.json.JsonUtil._
import uk.ac.wellcome.messaging.memory.MemoryMessageSender
import uk.ac.wellcome.messaging.worker.models.{
  NonDeterministicFailure,
  Successful
}
import uk.ac.wellcome.platform.archive.common.generators.IngestGenerators
import uk.ac.wellcome.platform.archive.common.ingests.models.{
  CallbackNotification,
  Ingest
}
import uk.ac.wellcome.platform.archive.common.ingests.models.Ingest.{
  Processing,
  Succeeded
}
import uk.ac.wellcome.platform.archive.common.ingests.tracker.fixtures.IngestTrackerFixtures
import uk.ac.wellcome.platform.archive.common.ingests.tracker.memory.MemoryIngestTracker
import uk.ac.wellcome.platform.archive.ingests.fixtures.IngestsFixtures

import scala.util.{Failure, Try}

class IngestsWorkerServiceTest
    extends FunSpec
    with IngestGenerators
    with IngestsFixtures
    with IngestTrackerFixtures
    with TryValues {

  describe("marking an ingest as completed") {
    val ingest = createIngest

    val ingestStatusUpdate =
      createIngestStatusUpdateWith(
        id = ingest.id,
        status = Succeeded
      )

    val expectedIngest = ingest.copy(
      status = Succeeded,
      events = ingestStatusUpdate.events
    )

    val callbackNotification = CallbackNotification(
      ingestId = ingest.id,
      callbackUri = ingest.callback.get.uri,
      payload = expectedIngest
    )

    val callbackNotificationMessageSender = new MemoryMessageSender()
    val updatedIngestsMessageSender = new MemoryMessageSender()

    implicit val ingestTracker: MemoryIngestTracker =
      createMemoryIngestTrackerWith(initialIngests = Seq(ingest))

    it("processes the message") {
      withIngestWorker(
        ingestTracker = ingestTracker,
        callbackNotificationMessageSender = callbackNotificationMessageSender,
        updatedIngestsMessageSender = updatedIngestsMessageSender
      ) {
        _.processMessage(ingestStatusUpdate).success.value shouldBe a[
          Successful[_]
        ]
      }
    }

    it("sends a callback notification message") {
      callbackNotificationMessageSender
        .getMessages[CallbackNotification] shouldBe Seq(
        callbackNotification
      )
    }

    it("records the ingest update in the tracker") {
      assertIngestRecordedRecentEvents(
        id = ingestStatusUpdate.id,
        expectedEventDescriptions = ingestStatusUpdate.events.map {
          _.description
        }
      )
    }

    it("sends a message with the updated ingest") {
      updatedIngestsMessageSender.getMessages[Ingest] shouldBe Seq(expectedIngest)
    }
  }

  describe("adding multiple events to an ingest") {
    val ingest = createIngestWith(
      status = Processing
    )

    val ingestStatusUpdate1 =
      createIngestStatusUpdateWith(
        id = ingest.id,
        status = Processing
      )

    val ingestStatusUpdate2 =
      createIngestStatusUpdateWith(
        id = ingest.id,
        status = Processing
      )

    val expectedIngest = ingest.copy(
      events = ingestStatusUpdate1.events ++ ingestStatusUpdate2.events
    )

    val callbackNotificationMessageSender = new MemoryMessageSender()
    val updatedIngestsMessageSender = new MemoryMessageSender()

    implicit val ingestTracker: MemoryIngestTracker =
      createMemoryIngestTrackerWith(initialIngests = Seq(ingest))

    it("processes both messages") {
      withIngestWorker(
        ingestTracker = ingestTracker,
        callbackNotificationMessageSender = callbackNotificationMessageSender,
        updatedIngestsMessageSender = updatedIngestsMessageSender
      ) { service =>
        service
          .processMessage(ingestStatusUpdate1)
          .success
          .value shouldBe a[Successful[_]]

        service
          .processMessage(ingestStatusUpdate2)
          .success
          .value shouldBe a[Successful[_]]
      }
    }

    it("adds the events to the ingest tracker") {
      ingestTracker
        .get(ingest.id)
        .right
        .value
        .identifiedT shouldBe expectedIngest
    }

    it("does not send a notification for ingests that are still processing") {
      callbackNotificationMessageSender.messages shouldBe empty
    }

    it("sends a message with the updated ingests") {
      val expectedIngest1 = ingest.copy(
        events = ingestStatusUpdate1.events
      )

      val expectedIngest2 = ingest.copy(
        events = ingestStatusUpdate1.events ++ ingestStatusUpdate2.events
      )

      updatedIngestsMessageSender.getMessages[Ingest] shouldBe Seq(expectedIngest1, expectedIngest2)
    }
  }

  it("fails if the ingest is not in the tracker") {
    withLocalSqsQueue { queue =>
      withMemoryIngestTracker() { implicit ingestTracker =>
        val messageSender = new MemoryMessageSender()
        withIngestWorker(queue, ingestTracker, messageSender) { service =>
          val ingestStatusUpdate =
            createIngestStatusUpdateWith(
              status = Succeeded
            )

          val result = service.processMessage(ingestStatusUpdate)
          result.success.value shouldBe a[NonDeterministicFailure[_]]

          result.success.value
            .asInstanceOf[NonDeterministicFailure[_]]
            .failure shouldBe a[Throwable]
        }
      }
    }
  }

  it("fails if publishing a message fails") {
    val ingest = createIngest

    withLocalSqsQueue { queue =>
      withMemoryIngestTracker(initialIngests = Seq(ingest)) {
        implicit ingestTracker =>
          val exception = new Throwable("BOOM!")

          val brokenSender = new MemoryMessageSender() {
            override def sendT[T](
              t: T
            )(implicit encoder: Encoder[T]): Try[Unit] =
              Failure(exception)
          }

          withIngestWorker(queue, ingestTracker, brokenSender) { service =>
            val ingestStatusUpdate =
              createIngestStatusUpdateWith(
                id = ingest.id,
                status = Succeeded
              )

            val result = service.processMessage(ingestStatusUpdate)

            result.success.value shouldBe a[NonDeterministicFailure[_]]
            result.success.value
              .asInstanceOf[NonDeterministicFailure[_]]
              .failure shouldBe exception
          }
      }
    }
  }
}
