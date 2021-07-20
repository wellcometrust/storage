package weco.storage_service.bag_verifier.fixity.azure

import software.amazon.awssdk.services.dynamodb.model.ScalarAttributeType

import java.net.URI
import weco.fixtures.TestWith
import weco.storage_service.bag_verifier.fixity.{
  FixityChecker,
  FixityCheckerTagsTestCases
}
import weco.storage_service.bag_verifier.storage.azure.{
  AzureLocatable,
  AzureResolvable
}
import weco.storage_service.checksum._
import weco.storage.azure.{AzureBlobLocation, AzureBlobLocationPrefix}
import weco.storage.fixtures.AzureFixtures.Container
import weco.storage.fixtures.DynamoFixtures.Table
import weco.storage.fixtures.{AzureFixtures, DynamoFixtures}
import weco.storage.services.azure.AzureSizeFinder
import weco.storage.store.azure.{AzureStreamStore, AzureTypedStore}

class AzureFixityCheckerTest
    extends FixityCheckerTagsTestCases[
      AzureBlobLocation,
      AzureBlobLocationPrefix,
      Container,
      Table,
      AzureStreamStore
    ]
    with DynamoFixtures
    with AzureFixtures {

  val azureTypedStore: AzureTypedStore[String] = AzureTypedStore[String]

  override def withContext[R](testWith: TestWith[Table, R]): R =
    withLocalDynamoDbTable { table =>
      testWith(table)
    }

  override def putString(location: AzureBlobLocation, contents: String)(
    implicit context: Table
  ): Unit = azureTypedStore.put(location)(contents)

  override def withFixityChecker[R](azureReader: AzureStreamStore)(
    testWith: TestWith[
      FixityChecker[AzureBlobLocation, AzureBlobLocationPrefix],
      R
    ]
  )(implicit table: Table): R = {
    val sizeFinder = new AzureSizeFinder()
    val tags = new AzureDynamoTags(createDynamoConfigWith(table))
    val locator = new AzureLocatable
    testWith(new AzureFixityChecker(azureReader, sizeFinder, tags, locator))
  }

  override def withStreamReader[R](
    testWith: TestWith[AzureStreamStore, R]
  )(implicit table: Table): R =
    testWith(new AzureStreamStore())

  override def resolve(location: AzureBlobLocation): URI =
    new AzureResolvable().resolve(location)

  override def withNamespace[R](testWith: TestWith[Container, R]): R =
    withAzureContainer { container =>
      testWith(container)
    }

  override def createId(implicit container: Container): AzureBlobLocation =
    createAzureBlobLocationWith(container)

  override def tagName(algorithm: ChecksumAlgorithm): String =
    algorithm match {
      case MD5    => "ContentMD5"
      case SHA1   => "ContentSHA1"
      case SHA256 => "ContentSHA256"
      case SHA512 => "ContentSHA512"
    }

  override def createTable(table: Table): Table =
    createTableWithHashKey(
      table,
      keyName = "id",
      keyType = ScalarAttributeType.S
    )
}
