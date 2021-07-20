package weco.storage_service.indexer.files.fixtures

import java.time.Instant
import com.sksamuel.elastic4s.Index
import io.circe.Decoder
import org.scalatest.Suite
import weco.fixtures.TestWith
import weco.json.JsonUtil._
import weco.messaging.fixtures.SQS.Queue
import weco.monitoring.memory.MemoryMetrics
import weco.storage_service.generators.StorageManifestGenerators
import weco.storage_service.storage.models.PrimaryS3StorageLocation
import weco.storage_service.indexer.models.FileContext
import weco.storage_service.indexer.{Indexer, IndexerWorker}
import weco.storage_service.indexer.files.models.IndexedFile
import weco.storage_service.indexer.files.{
  FileIndexer,
  FileIndexerWorker,
  FilesIndexConfig
}
import weco.storage_service.indexer.fixtures.IndexerFixtures
import weco.storage_service.indexer.elasticsearch.StorageServiceIndexConfig

import scala.concurrent.ExecutionContext.Implicits.global

trait FileIndexerFixtures
    extends IndexerFixtures[Seq[FileContext], FileContext, IndexedFile]
    with StorageManifestGenerators { this: Suite =>

  val indexConfig: StorageServiceIndexConfig = FilesIndexConfig

  def createContext: FileContext =
    FileContext(
      space = createStorageSpace,
      externalIdentifier = createExternalIdentifier,
      algorithm = createChecksum.algorithm,
      bagLocation = PrimaryS3StorageLocation(
        createS3ObjectLocationPrefix
      ),
      file = createStorageManifestFile,
      createdDate = Instant.now
    )

  def createT: (Seq[FileContext], String) = {
    val context = createContext

    (Seq(context), context.location.toString())
  }

  def createIndexer(index: Index): Indexer[FileContext, IndexedFile] =
    new FileIndexer(client = elasticClient, index = index)

  override def withIndexerWorker[R](index: Index, queue: Queue)(
    testWith: TestWith[
      IndexerWorker[Seq[FileContext], FileContext, IndexedFile],
      R
    ]
  )(implicit decoder: Decoder[Seq[FileContext]]): R = {
    withActorSystem { implicit actorSystem =>
      implicit val metrics: MemoryMetrics = new MemoryMetrics()

      val worker = new FileIndexerWorker(
        config = createAlpakkaSQSWorkerConfig(queue),
        indexer = createIndexer(index),
        metricsNamespace = "indexer"
      )

      testWith(worker)
    }
  }

  def convertToIndexedT(contexts: Seq[FileContext]): IndexedFile =
    IndexedFile(contexts.head)
}
