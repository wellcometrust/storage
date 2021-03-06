package weco.storage_service.indexer.fixtures

import com.sksamuel.elastic4s.ElasticDsl._
import com.sksamuel.elastic4s.requests.get.GetResponse
import com.sksamuel.elastic4s.requests.searches.SearchResponse
import com.sksamuel.elastic4s.requests.searches.queries.Query
import com.sksamuel.elastic4s.{Index, Response}
import io.circe.Decoder
import org.scalatest.Suite
import weco.json.JsonUtil.fromJson
import weco.elasticsearch.test.fixtures
import weco.fixtures.TestWith
import weco.storage_service.indexer.elasticsearch.StorageServiceIndexConfig

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

trait ElasticsearchFixtures extends fixtures.ElasticsearchFixtures {
  this: Suite =>

  protected val esHost = "localhost"
  protected val esPort = 9200

  def withLocalElasticsearchIndex[R](
    config: StorageServiceIndexConfig
  )(testWith: TestWith[Index, R]): R =
    withLocalElasticsearchIndex(config.config) { index =>
      testWith(index)
    }

  protected def getT[T](index: Index, id: String)(
    implicit decoder: Decoder[T]
  ): T = {
    val response: Response[GetResponse] =
      elasticClient.execute { get(index, id) }.await

    val getResponse = response.result
    getResponse.exists shouldBe true

    fromJson[T](getResponse.sourceAsString) match {
      case Success(t) => t
      case Failure(err) =>
        throw new Throwable(
          s"Unable to parse source string ($err): ${getResponse.sourceAsString}"
        )
    }
  }

  protected def searchT[T](index: Index, query: Query)(
    implicit decoder: Decoder[T]
  ): Seq[T] = {
    val response: Response[SearchResponse] =
      elasticClient.execute { search(index).query(query) }.await

    response.result.hits.hits
      .map { hit =>
        fromJson[T](hit.sourceAsString).get
      }
  }
}
