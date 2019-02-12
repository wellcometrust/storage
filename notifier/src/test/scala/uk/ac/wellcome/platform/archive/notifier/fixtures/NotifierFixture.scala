package uk.ac.wellcome.platform.archive.notifier.fixtures

import java.net.URL

import com.amazonaws.services.sns.model.PublishResult
import uk.ac.wellcome.akka.fixtures.Akka
import uk.ac.wellcome.fixtures.TestWith
import uk.ac.wellcome.messaging.fixtures.Messaging
import uk.ac.wellcome.messaging.fixtures.SNS.Topic
import uk.ac.wellcome.messaging.fixtures.SQS.Queue
import uk.ac.wellcome.messaging.sns.NotificationMessage
import uk.ac.wellcome.platform.archive.notifier.Notifier
import uk.ac.wellcome.platform.archive.common.fixtures.{ArchiveMessaging, BagIt}

trait NotifierFixture
    extends Akka
    with ArchiveMessaging
    with Messaging
    with BagIt {

  protected val callbackHost = "localhost"
  protected val callbackPort = 8080

  def withApp[R](queue: Queue, topic: Topic)(
    testWith: TestWith[Notifier, R]): R =
    withActorSystem { implicit actorSystem =>
      withMaterializer(actorSystem) { implicit materializer =>
        withArchiveMessageStream[NotificationMessage, PublishResult, R](queue) {
          messageStream =>
            val notifier = new Notifier(
              messageStream = messageStream,
              snsClient = snsClient,
              snsConfig = createSNSConfigWith(topic),
              contextUrl = new URL("http://localhost/context.json")
            )

            testWith(notifier)
        }
      }
    }

  def withNotifier[R](testWith: TestWith[(Queue, Topic, Notifier), R]): R =
    withLocalSqsQueue { queue =>
      withLocalSnsTopic { topic =>
        withApp(queue = queue, topic = topic) { app =>
          testWith((queue, topic, app))
        }
      }
    }
}
