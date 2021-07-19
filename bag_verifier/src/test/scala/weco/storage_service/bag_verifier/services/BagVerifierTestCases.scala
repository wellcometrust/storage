package weco.storage_service.bag_verifier.services

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, EitherValues, OptionValues, TryValues}
import weco.fixtures.TestWith
import weco.storage_service.bag_verifier.fixity.{
  FailedChecksumNoMatch,
  FileFixityCorrect
}
import weco.storage_service.bag_verifier.models._
import weco.storage_service.bag_verifier.storage.LocationNotFound
import weco.storage_service.bagit.models.{
  BagPath,
  BagVersion,
  ExternalIdentifier,
  PayloadOxum
}
import weco.storage_service.bagit.services.{BagReader, BagUnavailable}
import weco.storage_service.fixtures.{BagBuilder, PayloadEntry}
import weco.storage_service.generators.{
  BagInfoGenerators,
  StorageSpaceGenerators
}
import weco.storage_service.storage.models.{
  EnsureTrailingSlash,
  IngestFailed,
  IngestStepResult,
  IngestStepSucceeded,
  StorageSpace
}
import weco.storage.azure.AzureBlobLocation
import weco.storage.fixtures.S3Fixtures
import weco.storage.fixtures.S3Fixtures.Bucket
import weco.storage.s3.S3ObjectLocation
import weco.storage.store.TypedStore
import weco.storage.store.fixtures.NamespaceFixtures
import weco.storage.{Location, Prefix}

import scala.util.Random

trait BagVerifierTestCases[Verifier <: BagVerifier[
  BagContext,
  BagLocation,
  BagPrefix
], BagContext <: BagVerifyContext[BagPrefix], BagLocation <: Location, BagPrefix <: Prefix[
  BagLocation
], Namespace]
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with OptionValues
    with TryValues
    with StorageSpaceGenerators
    with BagInfoGenerators
    with NamespaceFixtures[BagLocation, Namespace]
    with S3Fixtures {

  implicit val et: EnsureTrailingSlash[BagPrefix]
  def withTypedStore[R](
    testWith: TestWith[TypedStore[BagLocation, String], R]
  ): R

  def withVerifier[R](primaryBucket: Bucket)(
    testWith: TestWith[Verifier, R]
  ): R

  def withBagContext[R](bagRoot: BagPrefix)(
    testWith: TestWith[BagContext, R]
  ): R

  val bagBuilder: BagBuilder[BagLocation, BagPrefix, Namespace]

  def withBag[R](
    space: StorageSpace,
    externalIdentifier: ExternalIdentifier,
    bagBuilder: BagBuilder[BagLocation, BagPrefix, Namespace] = bagBuilder,
    version: BagVersion = BagVersion(randomInt(from = 2, to = 10))
  )(
    testWith: TestWith[(Bucket, BagPrefix), R]
  )(implicit namespace: Namespace): R =
    withTypedStore { implicit typedStore =>
      withLocalS3Bucket { implicit primaryBucket =>
        val (bagRoot, _) = bagBuilder.storeBagWith(
          space = space,
          externalIdentifier = externalIdentifier,
          payloadFileCount = payloadFileCount,
          version = version
        )

        testWith((primaryBucket, bagRoot))
      }
    }

  val payloadFileCount: Int = randomInt(from = 1, to = 10)

  val expectedFileCount: Int = payloadFileCount + List(
    "manifest-sha256.txt",
    "bagit.txt",
    "bag-info.txt"
  ).size

  def writeFile(
    location: BagLocation,
    contents: String = randomAlphanumeric()
  ): Unit

  def createBagReader: BagReader[BagLocation, BagPrefix]

  trait BagBuilderImpl extends BagBuilder[BagLocation, BagPrefix, Namespace] {
    override implicit val typedStore: TypedStore[BagLocation, String] =
      bagBuilder.typedStore

    override def createBagRoot(
      space: StorageSpace,
      externalIdentifier: ExternalIdentifier,
      version: BagVersion
    )(
      namespace: Namespace
    ): BagPrefix =
      bagBuilder.createBagRoot(space, externalIdentifier, version)(
        namespace = namespace
      )

    override def createBagLocation(
      bagRoot: BagPrefix,
      path: String
    ): BagLocation =
      bagBuilder.createBagLocation(bagRoot, path)
  }

  it("passes a bag with correct checksum values") {
    val space = createStorageSpace
    val externalIdentifier = createExternalIdentifier
    withNamespace { implicit namespace =>
      withBag(space, externalIdentifier) {
        case (primaryBucket, bagRoot) =>
          val ingestStep =
            withBagContext(bagRoot) { bagContext =>
              withVerifier(primaryBucket) {
                _.verify(
                  ingestId = createIngestID,
                  bagContext = bagContext,
                  space = space,
                  externalIdentifier = externalIdentifier
                )
              }
            }

          val result = ingestStep.success.get

          result shouldBe a[IngestStepSucceeded[_]]
          result.summary shouldBe a[VerificationSuccessSummary]

          val summary = result.summary
            .asInstanceOf[VerificationSuccessSummary]
          val fixityListResult = summary.fixityListResult.value

          verifySuccessCount(
            fixityListResult.locations,
            expectedCount = expectedFileCount
          )
      }
    }
  }

  it("passes a bag with valid checksum values and Windows line endings") {
    val primaryBucket = createBucket

    val space = createStorageSpace
    val externalIdentifier = ExternalIdentifier("windows_line_endings_bag")

    withTypedStore { implicit typedStore =>
      withNamespace { implicit namespace =>
        val bagRoot = bagBuilder.createBagRoot(
          space = space,
          externalIdentifier = externalIdentifier
        )(
          namespace = namespace
        )

        val bagContents = bagBuilder.BagContents(
          fetchObjects = Map(),
          bagObjects = Map(
            bagRoot.asLocation("data/README.txt") ->
              "This is a file with Windows line endings.\r\n",
            bagRoot.asLocation("bag-info.txt") -> (
              "Bag-Software-Agent: bagit.py v1.7.0 <https://github.com/LibraryOfCongress/bagit-python>\r\n" +
                "Bagging-Date: 2021-07-16\r\n" +
                "External-Identifier: windows_line_endings_bag\r\n" +
                "Payload-Oxum: 43.1\r\n"
            ),
            bagRoot.asLocation("bagit.txt") -> (
              "BagIt-Version: 0.97\r\n" +
                "Tag-File-Character-Encoding: UTF-8\r\n"
            ),
            bagRoot.asLocation("manifest-sha256.txt") ->
              "3145c4eacf0ff758f80ed68f6c6fa8d94ec3d31b1b7f421b8dc4a1f61973f4c8  data/README.txt\r\n",
            bagRoot.asLocation("tagmanifest-sha256.txt") -> (
              "c2c8f2cf0726d04b0dab99d67f951d77a33ffe6eae2a46cee67dbb2bccb96b0d  bag-info.txt\r\n" +
                "4ec18508e945726d1bdbdbdfef3e8973ede85fef1f4839bc166883dc958fbe93  bagit.txt\r\n" +
                "d1d9d610ceeaee50b744d06731bdc7dffa5b24ea59987ab57ba46489550e9118  manifest-sha256.txt\r\n",
            )
          ),
          bagRoot = bagRoot,
          bagInfo = createBagInfoWith(externalIdentifier = externalIdentifier)
        )

        bagBuilder.storeBagContents(bagContents)

        val ingestStep =
          withBagContext(bagRoot) { bagContext =>
            withVerifier(primaryBucket) {
              _.verify(
                ingestId = createIngestID,
                bagContext = bagContext,
                space = space,
                externalIdentifier = externalIdentifier
              )
            }
          }

        val result = ingestStep.success.get

        result shouldBe a[IngestStepSucceeded[_]]
        result.summary shouldBe a[VerificationSuccessSummary]

        val summary = result.summary
          .asInstanceOf[VerificationSuccessSummary]
        val fixityListResult = summary.fixityListResult.value

        verifySuccessCount(
          fixityListResult.locations,
          expectedCount = 4
        )
      }
    }
  }

  it("passes a bag with uppercase checksum values") {
    val space = createStorageSpace
    val externalIdentifier = ExternalIdentifier("uppercase_checksums")

    val uppercaseBuilder = new BagBuilderImpl {
      override protected def createDigest(string: String): String =
        super.createDigest(string).toUpperCase
    }

    withNamespace { implicit namespace =>
      withBag(space, externalIdentifier, bagBuilder = uppercaseBuilder) {
        case (primaryBucket, bagRoot) =>
          val ingestStep =
            withBagContext(bagRoot) { bagContext =>
              withVerifier(primaryBucket) {
                _.verify(
                  ingestId = createIngestID,
                  bagContext = bagContext,
                  space = space,
                  externalIdentifier = externalIdentifier
                )
              }
            }

          val result = ingestStep.success.get

          result shouldBe a[IngestStepSucceeded[_]]
          result.summary shouldBe a[VerificationSuccessSummary]
      }
    }
  }

  it("fails a bag with an incorrect checksum in the file manifest") {
    val badBuilder: BagBuilderImpl = new BagBuilderImpl {
      override protected def createPayloadManifest(
        entries: Seq[PayloadEntry]
      ): Option[String] =
        super.createPayloadManifest(
          entries.head.copy(contents = randomAlphanumeric()) +: entries.tail
        )
    }

    assertBagFails(badBuilder) {
      case (_, summary) =>
        val fixityListResult = summary.fixityListResult.value

        verifySuccessCount(
          fixityListResult.correct,
          expectedCount = expectedFileCount - 1
        )
        fixityListResult.errors should have size 1

        val fixityError = fixityListResult.errors.head
        val error = fixityError.e

        error shouldBe a[FailedChecksumNoMatch]
        error.getMessage should include("Checksum values do not match!")
    }
  }

  it("fails a bag with multiple incorrect checksums in the file manifest") {
    val badBuilder = new BagBuilderImpl {
      override protected def createPayloadManifest(
        entries: Seq[PayloadEntry]
      ): Option[String] =
        super.createPayloadManifest(
          entries.map { _.copy(contents = randomAlphanumeric()) }
        )
    }

    assertBagFails(badBuilder) {
      case (ingestFailed, _) =>
        ingestFailed.maybeUserFacingMessage.get should startWith(
          s"Unable to verify $payloadFileCount files in the bag:"
        )
    }
  }

  it("fails a bag if the file manifest refers to a non-existent file") {
    val badBuilder = new BagBuilderImpl {
      override protected def createPayloadManifest(
        entries: Seq[PayloadEntry]
      ): Option[String] =
        super.createPayloadManifest(
          entries.tail :+ PayloadEntry(
            bagPath = BagPath(s"data/$randomPath"),
            path = randomAlphanumeric(),
            contents = randomAlphanumeric()
          )
        )

      // This ensures that the fetch file won't refer to the entry
      // we've deleted from the manifest.
      override protected def getFetchEntryCount(payloadFileCount: Int): Int = 0
    }

    assertBagFails(badBuilder) {
      case (_, summary) =>
        val fixityListResult = summary.fixityListResult.value

        verifySuccessCount(
          fixityListResult.correct,
          expectedCount = expectedFileCount - 1
        )
        fixityListResult.errors should have size 1

        val fixityError = fixityListResult.errors.head
        val error = fixityError.e

        error shouldBe a[LocationNotFound[_]]
        error.getMessage should startWith("Location not available!")
    }
  }

  it("fails a bag if the file manifest does not exist") {
    val badBuilder = new BagBuilderImpl {
      override protected def createPayloadManifest(
        entries: Seq[PayloadEntry]
      ): Option[String] =
        None
    }

    assertBagIncomplete(badBuilder) {
      case (ingestFailed, summary) =>
        val error = summary.e

        error shouldBe a[BagUnavailable]
        error.getMessage should include(
          "Could not find any payload manifests in the bag"
        )

        ingestFailed.maybeUserFacingMessage.get shouldBe "Could not find any payload manifests in the bag"
    }
  }

  it("fails a bag if the tag manifest does not exist") {
    val badBuilder = new BagBuilderImpl {
      override protected def createTagManifest(
        entries: Seq[ManifestFile]
      ): Option[String] =
        None
    }

    assertBagIncomplete(badBuilder) {
      case (ingestFailed, summary) =>
        val error = summary.e

        error shouldBe a[BagUnavailable]
        error.getMessage should include(
          "Could not find any tag manifests in the bag"
        )

        ingestFailed.maybeUserFacingMessage.get shouldBe "Could not find any tag manifests in the bag"
    }
  }

  it("fails a bag if the bag declaration does not exist") {
    val badBuilder = new BagBuilderImpl {
      override def createBagDeclaration: Option[ManifestFile] = None
    }

    assertBagIncomplete(badBuilder) {
      case (ingestFailed, _) =>
        ingestFailed.maybeUserFacingMessage.get shouldBe "Error loading Bag Declaration (bagit.txt): no such file!"
    }
  }

  it("fails a bag if there are payload files outside the data/ directory") {
    val badBuilder = new BagBuilderImpl {
      override protected def createPayloadFiles(
        space: StorageSpace,
        externalIdentifier: ExternalIdentifier,
        version: BagVersion,
        payloadFileCount: Int,
        isFetch: Boolean
      ): Seq[PayloadEntry] = {
        val bagRoot = createBagRootPath(space, externalIdentifier, version)

        // We don't want to put these files in the fetch.txt, or we'll get a
        // different error:
        //
        //    Error loading fetch.txt: fetch.txt should not contain tag files
        //
        val badFiles =
          if (isFetch) {
            Seq()
          } else {
            (1 to 3).map { _ =>
              val bagPath = BagPath(randomPath)
              PayloadEntry(
                bagPath = bagPath,
                path = s"$bagRoot/$bagPath",
                contents = Random.nextString(length = randomInt(1, 256))
              )
            }
          }

        super.createPayloadFiles(
          space,
          externalIdentifier,
          version,
          payloadFileCount,
          isFetch
        ) ++ badFiles
      }
    }

    assertBagIncomplete(badBuilder) {
      case (ingestFailed, _) =>
        ingestFailed.maybeUserFacingMessage.get should startWith(
          "Not all payload files are in the data/ directory"
        )
    }
  }

  it("fails a bag if there are tag files outside the root directory") {
    val badBuilder = new BagBuilderImpl {
      override protected def createTagManifest(
        entries: Seq[ManifestFile]
      ): Option[String] =
        super.createTagManifest(
          entries ++ Seq(
            ManifestFile("data/bagit.txt", contents = "123"),
            ManifestFile("tags/metadata.csv", contents = "123")
          )
        )
    }

    assertBagIncomplete(badBuilder) {
      case (ingestFailed, _) =>
        ingestFailed.maybeUserFacingMessage.get should startWith(
          "Not all tag files are in the root directory:"
        )
    }
  }

  it("fails if the external identifier in the bag-info.txt is incorrect") {
    val space = createStorageSpace
    val externalIdentifier = createExternalIdentifier.underlying
    val bagInfoExternalIdentifier =
      ExternalIdentifier(externalIdentifier + "_bag-info")
    val payloadExternalIdentifier =
      ExternalIdentifier(externalIdentifier + "_payload")

    withNamespace { implicit namespace =>
      withBag(space, bagInfoExternalIdentifier) {
        case (primaryBucket, bagRoot) =>
          val ingestStep =
            withBagContext(bagRoot) { bagContext =>
              withVerifier(primaryBucket) {
                _.verify(
                  ingestId = createIngestID,
                  bagContext = bagContext,
                  space = space,
                  externalIdentifier = payloadExternalIdentifier
                )
              }
            }

          val result = ingestStep.success.get

          result shouldBe a[IngestFailed[_]]
          result.summary shouldBe a[VerificationIncompleteSummary]

          result.maybeUserFacingMessage.get should startWith(
            "External identifier in bag-info.txt does not match request"
          )
      }
    }
  }

  describe("handles multiple manifests") {
    it("passes a bag with two manifests") {
      val space = createStorageSpace
      val externalIdentifier = ExternalIdentifier("multiple_manifests")
      val version = createBagVersion

      withNamespace { namespace =>
        val bagRoot =
          bagBuilder.createBagRoot(space, externalIdentifier, version)(
            namespace
          )

        val bagInfo = createBagInfoWith(
          payloadOxum = PayloadOxum(payloadBytes = 15, numberOfPayloadFiles = 1),
          externalIdentifier = externalIdentifier
        )

        val bagContents = bagBuilder.BagContents(
          fetchObjects = Map(),
          bagObjects = Map(
            bagRoot.asLocation("data/README.txt") -> "This is a file\n",
            bagRoot.asLocation("bag-info.txt") -> (
              "Bagging-Date: 2021-07-16\n" +
                "External-Identifier: multiple_manifests\n" +
                "Payload-Oxum: 15.1\n"
            ),
            bagRoot.asLocation("bagit.txt") -> (
              "BagIt-Version: 0.97\n" +
                "Tag-File-Character-Encoding: UTF-8\n"
            ),
            bagRoot
              .asLocation("manifest-sha512.txt") -> "7cd31c95fc5a40e5be7bf46e84df52c6d8d50e9003dfb7e3b85ac9c704b90a63ac220147645ff22d410166356133d241a7346e452c863601ce68b82d075031f8  data/README.txt\n",
            bagRoot
              .asLocation("manifest-md5.txt") -> "a86e2699931d4f3d1456e79383749e43  data/README.txt\n",
            bagRoot.asLocation("tagmanifest-sha512.txt") -> (
              "b7112a34f6892c1d3bfb6054dc4977c2ffd32bd7e4d8b686d08f68d1ef407c35857ad3cf552543318238701afb390faad20ac7a0a22b1cf43cd916dfb5d97efa  bag-info.txt\n" +
                "418dcfbe17d5f4b454b18630be795462cf7da4ceb6313afa49451aa2568e41f7ca3d34cf0280c7d056dc5681a70c37586aa1755620520b9198eede905ba2d0f6  bagit.txt\n" +
                "bfbd969850673f65d14917bcbe42e86df867e4e383702a4471eb0776f2f1cfa48ec102489416741dcf278344bc0229ac2a9011080ffe2a4e55a64540ed0291d9  manifest-sha512.txt\n" +
                "f8036c779eba074e72101458d675c287b731f5bec4cbe744d59565ce4cc26f96d5259d8f7b1cc55f3999d4db34eba59d99dc131200f1bdf8ddc89912ed23afe6  manifest-md5.txt\n"
            ),
            bagRoot.asLocation("tagmanifest-md5.txt") -> (
              "aa3c5e977224a9186dbb36ef1193be0d  bag-info.txt\n" +
                "9e5ad981e0d29adc278f6a294b8c2aca  bagit.txt\n" +
                "d570da37be627c3955c165422e667245  manifest-sha512.txt\n" +
                "7983626d0844789acfe8059b6730b9d1  manifest-md5.txt\n"
            )
          ),
          bagRoot = bagRoot,
          bagInfo = bagInfo
        )

        withTypedStore { implicit typedStore =>
          bagBuilder.storeBagContents(bagContents)

          val primaryBucket = createBucket

          val ingestStep =
            withBagContext(bagRoot) { bagContext =>
              withVerifier(primaryBucket) {
                _.verify(
                  ingestId = createIngestID,
                  bagContext = bagContext,
                  space = space,
                  externalIdentifier = externalIdentifier
                )
              }
            }

          ingestStep.success.get shouldBe a[IngestStepSucceeded[_]]
        }
      }
    }

    it("fails if one of the payload manifests has an incorrect checksum") {
      val space = createStorageSpace
      val externalIdentifier = ExternalIdentifier("multiple_manifests")
      val version = createBagVersion

      withNamespace { namespace =>
        val bagRoot =
          bagBuilder.createBagRoot(space, externalIdentifier, version)(
            namespace
          )

        val bagInfo = createBagInfoWith(
          payloadOxum = PayloadOxum(payloadBytes = 15, numberOfPayloadFiles = 1),
          externalIdentifier = externalIdentifier
        )

        val bagContents = bagBuilder.BagContents(
          fetchObjects = Map(),
          bagObjects = Map(
            bagRoot.asLocation("data/README.txt") -> "This is a file\n",
            bagRoot.asLocation("bag-info.txt") -> (
              "Bagging-Date: 2021-07-16\n" +
                "External-Identifier: multiple_manifests\n" +
                "Payload-Oxum: 15.1\n"
            ),
            bagRoot.asLocation("bagit.txt") -> (
              "BagIt-Version: 0.97\n" +
                "Tag-File-Character-Encoding: UTF-8\n"
            ),
            bagRoot
              .asLocation("manifest-sha512.txt") -> "7cd31c95fc5a40e5be7bf46e84df52c6d8d50e9003dfb7e3b85ac9c704b90a63ac220147645ff22d410166356133d241a7346e452c863601ce68b82d075031f8  data/README.txt\n",
            bagRoot
              .asLocation("manifest-md5.txt") -> "aaaaaaaa  data/README.txt\n",
            bagRoot.asLocation("tagmanifest-sha512.txt") -> (
              "b7112a34f6892c1d3bfb6054dc4977c2ffd32bd7e4d8b686d08f68d1ef407c35857ad3cf552543318238701afb390faad20ac7a0a22b1cf43cd916dfb5d97efa  bag-info.txt\n" +
                "418dcfbe17d5f4b454b18630be795462cf7da4ceb6313afa49451aa2568e41f7ca3d34cf0280c7d056dc5681a70c37586aa1755620520b9198eede905ba2d0f6  bagit.txt\n" +
                "bfbd969850673f65d14917bcbe42e86df867e4e383702a4471eb0776f2f1cfa48ec102489416741dcf278344bc0229ac2a9011080ffe2a4e55a64540ed0291d9  manifest-sha512.txt\n" +
                "f8036c779eba074e72101458d675c287b731f5bec4cbe744d59565ce4cc26f96d5259d8f7b1cc55f3999d4db34eba59d99dc131200f1bdf8ddc89912ed23afe6  manifest-md5.txt\n"
            ),
            bagRoot.asLocation("tagmanifest-md5.txt") -> (
              "aa3c5e977224a9186dbb36ef1193be0d  bag-info.txt\n" +
                "9e5ad981e0d29adc278f6a294b8c2aca  bagit.txt\n" +
                "d570da37be627c3955c165422e667245  manifest-sha512.txt\n" +
                "7983626d0844789acfe8059b6730b9d1  manifest-md5.txt\n"
            )
          ),
          bagRoot = bagRoot,
          bagInfo = bagInfo
        )

        withTypedStore { implicit typedStore =>
          bagBuilder.storeBagContents(bagContents)

          val primaryBucket = createBucket

          val ingestStep =
            withBagContext(bagRoot) { bagContext =>
              withVerifier(primaryBucket) {
                _.verify(
                  ingestId = createIngestID,
                  bagContext = bagContext,
                  space = space,
                  externalIdentifier = externalIdentifier
                )
              }
            }

          val result = ingestStep.success.get

          result shouldBe a[IngestFailed[_]]
          result.maybeUserFacingMessage shouldBe Some(
            "Unable to verify 2 files in the bag: manifest-md5.txt, data/README.txt"
          )
        }
      }
    }
  }

  describe("checks the fetch file") {
    it("fails if the fetch file refers to a file not in the manifest") {
      val badBuilder = new BagBuilderImpl {
        override protected def createFetchFile(
          primaryBucket: Bucket,
          entries: Seq[PayloadEntry]
        ): Option[String] =
          super.createFetchFile(
            primaryBucket,
            entries :+ entries.head.copy(
              bagPath = BagPath(entries.head.bagPath + "_extra"),
              path = entries.head.path + "_extra",
              contents = randomAlphanumeric()
            )
          )
      }

      assertBagIncomplete(badBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "fetch.txt refers to paths that aren't in the bag manifest: "
          )

          ingestFailed.maybeUserFacingMessage.get should startWith(
            "fetch.txt refers to paths that aren't in the bag manifest: "
          )
      }
    }

    it("fails if the fetch file refers to a file with the wrong URI scheme") {
      val wrongSchemeBuilder = new BagBuilderImpl {
        override def buildFetchEntryLine(
          primaryBucket: Bucket,
          entry: PayloadEntry
        ): String =
          super
            .buildFetchEntryLine(primaryBucket, entry)
            .replace("s3://", "none://")

        override protected def getFetchEntryCount(payloadFileCount: Int): Int =
          payloadFileCount
      }

      assertBagIncomplete(wrongSchemeBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )

          ingestFailed.maybeUserFacingMessage.get should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )
      }
    }

    it("fails if the fetch file refers to a file in a different bucket") {
      val wrongBucket = createBucket
      val wrongBucketFetchBuilder = new BagBuilderImpl {
        override def buildFetchEntryLine(
          primaryBucket: Bucket,
          entry: PayloadEntry
        ): String =
          super.buildFetchEntryLine(wrongBucket, entry)

        override protected def getFetchEntryCount(
          payloadFileCount: Int
        ): Int =
          payloadFileCount
      }

      assertBagIncomplete(wrongBucketFetchBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )

          ingestFailed.maybeUserFacingMessage.get should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )
      }
    }

    it("fails if the fetch file refers to a file in the wrong space") {
      val bagSpaceFetchBuilder = new BagBuilderImpl {
        override def buildFetchEntryLine(
          primaryBucket: Bucket,
          entry: PayloadEntry
        ): String =
          super.buildFetchEntryLine(
            primaryBucket,
            entry.copy(
              path = "badspace_" + entry.path
            )
          )

        override protected def getFetchEntryCount(payloadFileCount: Int): Int =
          payloadFileCount
      }

      assertBagIncomplete(bagSpaceFetchBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )

          ingestFailed.maybeUserFacingMessage.get should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )
      }
    }

    it(
      "fails if the fetch file refers to a file with the wrong external identifier"
    ) {
      val badExternalIdentifierFetchBuilder = new BagBuilderImpl {
        override def buildFetchEntryLine(
          primaryBucket: Bucket,
          entry: PayloadEntry
        ): String =
          super.buildFetchEntryLine(
            primaryBucket,
            entry.copy(
              path = entry.path.replace("/", "/wrong_")
            )
          )

        override protected def getFetchEntryCount(payloadFileCount: Int): Int =
          payloadFileCount
      }

      assertBagIncomplete(badExternalIdentifierFetchBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )

          ingestFailed.maybeUserFacingMessage.get should startWith(
            "fetch.txt refers to paths in a mismatched prefix or with a non-S3 URI scheme:"
          )
      }
    }
  }

  describe("checks for unreferenced files") {
    it("fails if there is one unreferenced file") {
      val badBuilder = new BagBuilderImpl {
        override def storeBagContents(
          bagContents: BagContents
        )(implicit typedStore: TypedStore[BagLocation, String]): Unit = {
          super.storeBagContents(bagContents)

          val location = bagContents.bagRoot.asLocation("unreferencedfile.txt")
          writeFile(location)
        }
      }

      assertBagIncomplete(badBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "Bag contains a file which is not referenced in the manifest:"
          )

          ingestFailed.maybeUserFacingMessage.get shouldBe
            "Bag contains a file which is not referenced in the manifest: /unreferencedfile.txt"
      }
    }

    it("fails if there are multiple unreferenced files") {
      val badBuilder = new BagBuilderImpl {
        override def storeBagContents(
          bagContents: BagContents
        )(implicit typedStore: TypedStore[BagLocation, String]): Unit = {
          super.storeBagContents(bagContents)

          (1 to 3).foreach { i =>
            val location =
              bagContents.bagRoot.asLocation(s"unreferencedfile_$i.txt")
            writeFile(location)
          }
        }
      }

      assertBagIncomplete(badBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "Bag contains 3 files which are not referenced in the manifest:"
          )

          ingestFailed.maybeUserFacingMessage.get shouldBe
            s"Bag contains 3 files which are not referenced in the manifest: " +
              "/unreferencedfile_1.txt, /unreferencedfile_2.txt, /unreferencedfile_3.txt"
      }
    }

    it("fails if a file in the fetch.txt also appears in the bag") {
      val alwaysWriteAsFetchBuilder = new BagBuilderImpl {
        override def storeBagContents(
          bagContents: BagContents
        )(implicit typedStore: TypedStore[BagLocation, String]): Unit = {
          super.storeBagContents(bagContents)

          val bagRoot = bagContents.bagRoot
          val bag = createBagReader.get(bagRoot).value

          // Write one of the fetch.txt entries as a concrete file
          val badFetchPath: BagPath = bag.fetch.get.paths.head
          val badFetchLocation = bagRoot.asLocation(badFetchPath.value)
          writeFile(badFetchLocation)
        }
      }

      assertBagIncomplete(alwaysWriteAsFetchBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.e.getMessage should startWith(
            "Files referred to in the fetch.txt also appear in the bag:"
          )

          ingestFailed.maybeUserFacingMessage.get should startWith(
            "Files referred to in the fetch.txt also appear in the bag:"
          )
      }
    }
  }

  describe("checks the Payload-Oxum") {
    it("fails if the Payload-Oxum has the wrong file count") {
      val badBuilder = new BagBuilderImpl {
        override protected def createPayloadOxum(
          entries: Seq[PayloadEntry]
        ): PayloadOxum = {
          val oxum = super.createPayloadOxum(entries)

          oxum.copy(numberOfPayloadFiles = oxum.numberOfPayloadFiles - 1)
        }
      }

      assertBagIncomplete(badBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.maybeUserFacingMessage.get shouldBe
            s"""Payload-Oxum has the wrong number of payload files: ${payloadFileCount - 1}, but bag manifest has $payloadFileCount"""
      }
    }

    it("fails if the Payload-Oxum has the wrong octet count") {
      val badBuilder = new BagBuilderImpl {
        override protected def createPayloadOxum(
          entries: Seq[PayloadEntry]
        ): PayloadOxum = {
          val oxum = super.createPayloadOxum(entries)

          oxum.copy(payloadBytes = oxum.payloadBytes - 1)
        }
      }

      assertBagIncomplete(badBuilder) {
        case (ingestFailed, _) =>
          ingestFailed.maybeUserFacingMessage.get should fullyMatch regex
            s"""Payload-Oxum has the wrong octetstream sum: \\d+ bytes, but bag actually contains \\d+ bytes"""
      }
    }
  }

  describe("checks for illegal filenames") {
    it("fails if the manifest has illegal filenames") {
      val badBuilder = new BagBuilderImpl {
        override protected def randomPath: _root_.scala.Predef.String =
          super.randomPath + "."
      }

      assertBagResultFails(badBuilder) { result =>
        result.summary shouldBe a[VerificationIncompleteSummary]

        val summary = result.summary.asInstanceOf[VerificationIncompleteSummary]

        summary.e.getMessage should startWith("Filenames cannot end with a .:")
      }
    }

    it("fails if the tag manifest has illegal filenames") {
      val badBuilder = new BagBuilderImpl {
        override protected def createTagManifest(
          entries: Seq[ManifestFile]
        ): Option[String] =
          super.createTagManifest(
            entries.map { file =>
              file.copy(name = file.name + ".")
            }
          )
      }

      assertBagResultFails(badBuilder) { result =>
        result.summary shouldBe a[VerificationIncompleteSummary]

        val summary = result.summary.asInstanceOf[VerificationIncompleteSummary]

        summary.e.getMessage should startWith("Filenames cannot end with a .:")
      }
    }
  }

  it("skips locations in a namespace with same prefix but different directory") {
    val space = createStorageSpace
    val externalIdentifier = createExternalIdentifier

    withNamespace { implicit namespace =>
      //put a bag in $space/$externalIdentifier/v1
      withBag(space, externalIdentifier, version = BagVersion(1)) {
        case (primaryBucket, bagRoot) =>
          //put another version of the bag in $space/$externalIdentifier/v10
          withBag(space, externalIdentifier, version = BagVersion(10)) { _ =>
            val ingestStep =
              withBagContext(bagRoot) { bagContext =>
                withVerifier(primaryBucket) {
                  _.verify(
                    ingestId = createIngestID,
                    bagContext = bagContext,
                    space = space,
                    externalIdentifier = externalIdentifier
                  )
                }
              }

            val result = ingestStep.success.get

            result shouldBe a[IngestStepSucceeded[_]]
            result.summary shouldBe a[VerificationSuccessSummary]

            val summary = result.summary
              .asInstanceOf[VerificationSuccessSummary]
            val fixityListResult = summary.fixityListResult.value

            verifySuccessCount(
              fixityListResult.locations,
              expectedCount = expectedFileCount
            )
          }
      }
    }
  }

  private def verifySuccessCount(
    successes: List[FileFixityCorrect[_]],
    expectedCount: Int
  ): Assertion =
    if (successes
          .map { fixityEntry =>
            fixityEntry.objectLocation match {
              case azureBlobLocation: AzureBlobLocation =>
                azureBlobLocation.name
              case s3ObjectLocation: S3ObjectLocation => s3ObjectLocation.key
            }
          }
          .exists { _.endsWith("/fetch.txt") }) {
      successes.size shouldBe expectedCount + 1
    } else {
      successes.size shouldBe expectedCount
    }

  private def assertBagFails(badBuilder: BagBuilderImpl)(
    assertion: (
      IngestFailed[VerificationFailureSummary],
      VerificationFailureSummary
    ) => Assertion
  ): Assertion =
    assertBagResultFails(badBuilder) { result =>
      result.summary shouldBe a[VerificationFailureSummary]

      val failedResult =
        result.asInstanceOf[IngestFailed[VerificationFailureSummary]]
      val summary = result.summary.asInstanceOf[VerificationFailureSummary]

      assertion(failedResult, summary)
    }

  // Given a builder that fails to create a valid bag for some reason, ensure that
  // it is caught correctly by the verifier.
  private def assertBagResultFails(
    badBuilder: BagBuilderImpl
  )(
    assertion: IngestStepResult[VerificationSummary] => Assertion
  ): Assertion = {
    val space = createStorageSpace
    val externalIdentifier = createExternalIdentifier

    withNamespace { implicit namespace =>
      withBag(space, externalIdentifier, bagBuilder = badBuilder) {
        case (primaryBucket, bagRoot) =>
          val ingestStep =
            withBagContext(bagRoot) { bagContext =>
              withVerifier(primaryBucket) {
                _.verify(
                  ingestId = createIngestID,
                  bagContext = bagContext,
                  space = space,
                  externalIdentifier = externalIdentifier
                )
              }
            }

          val result = ingestStep.success.get
          debug(s"result = $result")

          result shouldBe a[IngestFailed[_]]
          assertion(result)
      }
    }
  }

  private def assertBagIncomplete(badBuilder: BagBuilderImpl)(
    assertion: (
      IngestFailed[VerificationIncompleteSummary],
      VerificationIncompleteSummary
    ) => Assertion
  ): Assertion =
    assertBagResultFails(badBuilder) { result =>
      result.summary shouldBe a[VerificationIncompleteSummary]

      val failedResult =
        result.asInstanceOf[IngestFailed[VerificationIncompleteSummary]]
      val summary = result.summary.asInstanceOf[VerificationIncompleteSummary]

      assertion(failedResult, summary)
    }
}
