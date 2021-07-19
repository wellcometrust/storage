package weco.storage_service.bagit.services

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.{Assertion, EitherValues}
import weco.fixtures.TestWith
import weco.storage.fixtures.S3Fixtures
import weco.storage.fixtures.S3Fixtures.Bucket
import weco.storage.store.TypedStore
import weco.storage.{Location, Prefix}
import weco.storage_service.bagit.models._
import weco.storage_service.fixtures.BagBuilder
import weco.storage_service.generators.StorageRandomGenerators
import weco.storage_service.verify.ChecksumValue

import java.time.LocalDate

trait BagReaderTestCases[
  Context,
  Namespace,
  BagLocation <: Location,
  BagPrefix <: Prefix[BagLocation]
] extends AnyFunSpec
    with Matchers
    with EitherValues
    with StorageRandomGenerators
    with BagBuilder[BagLocation, BagPrefix, Namespace]
    with S3Fixtures {

  def withContext[R](testWith: TestWith[Context, R]): R
  def withTypedStore[R](
    testWith: TestWith[TypedStore[BagLocation, String], R]
  )(implicit context: Context): R

  def withBagReader[R](
    testWith: TestWith[BagReader[BagLocation, BagPrefix], R]
  )(
    implicit context: Context
  ): R

  def withNamespace[R](testWith: TestWith[Namespace, R]): R

  def deleteFile(root: BagPrefix, path: String)(
    implicit context: Context
  )

  def scrambleFile(root: BagPrefix, path: String)(
    implicit typedStore: TypedStore[BagLocation, String]
  ): Assertion =
    typedStore.put(root.asLocation(path))(randomAlphanumeric()) shouldBe a[
      Right[_, _]
    ]

  def withFixtures[R](
    testWith: TestWith[
      (Context, TypedStore[BagLocation, String], Namespace, Bucket),
      R
    ]
  ): R =
    withContext { implicit context =>
      withTypedStore { typedStore =>
        withNamespace { namespace =>
          withLocalS3Bucket { bucket =>
            testWith((context, typedStore, namespace, bucket))
          }
        }
      }
    }

  it("gets a correctly formed bag") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, bagInfo) = createBag()(namespace, bucket)

      val bag = withBagReader {
        _.get(bagRoot).value
      }

      bag.info shouldBe bagInfo
    }
  }

  it("gets a bag with multiple manifests") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val space = createStorageSpace
      val externalIdentifier = ExternalIdentifier("multiple_manifests")
      val version = createBagVersion

      val bagRoot: BagPrefix =
        createBagRoot(space, externalIdentifier, version)(namespace)

      val bagInfo = BagInfo(
        payloadOxum = PayloadOxum(payloadBytes = 15, numberOfPayloadFiles = 1),
        externalIdentifier = externalIdentifier,
        baggingDate = LocalDate.now()
      )

      val bagContents = BagContents(
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

      storeBagContents(bagContents)

      val bag = withBagReader {
        _.get(bagRoot).value
      }

      bag.manifest shouldBe NewPayloadManifest(
        entries = Map(
          BagPath("data/README.txt") -> MultiChecksumValue(
            md5 = Some(ChecksumValue("a86e2699931d4f3d1456e79383749e43")),
            sha1 = None,
            sha256 = None,
            sha512 = Some(
              ChecksumValue(
                "7cd31c95fc5a40e5be7bf46e84df52c6d8d50e9003dfb7e3b85ac9c704b90a63ac220147645ff22d410166356133d241a7346e452c863601ce68b82d075031f8"
              )
            )
          )
        )
      )
    }
  }

  it("errors if the bag-info.txt file does not exist") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      deleteFile(bagRoot, path = "bag-info.txt")

      withBagReader {
        _.get(bagRoot).left.value.msg should startWith(
          "Error loading bag-info.txt"
        )
      }
    }
  }

  it("errors if the bag-info.txt file is malformed") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      scrambleFile(bagRoot, path = "bag-info.txt")

      withBagReader {
        _.get(bagRoot).left.value.msg should startWith(
          "Error loading bag-info.txt"
        )
      }
    }
  }

  it("errors if there is no payload manifest") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      deleteFile(bagRoot, path = "manifest-sha256.txt")

      withBagReader {
        _.get(bagRoot).left.value.msg shouldBe "Could not find any payload manifests in the bag"
      }
    }
  }

  it("errors if the payload manifest is malformed") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      scrambleFile(bagRoot, path = "manifest-sha256.txt")

      withBagReader {
        _.get(bagRoot).left.value.msg should startWith(
          "Error loading manifest-sha256.txt"
        )
      }
    }
  }

  it("errors if the payload manifests have different files") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val space = createStorageSpace
      val externalIdentifier = ExternalIdentifier("multiple_manifests")
      val version = createBagVersion

      val bagRoot: BagPrefix =
        createBagRoot(space, externalIdentifier, version)(namespace)

      val bagInfo = BagInfo(
        payloadOxum = PayloadOxum(payloadBytes = 15, numberOfPayloadFiles = 1),
        externalIdentifier = externalIdentifier,
        baggingDate = LocalDate.now()
      )

      val bagContents = BagContents(
        fetchObjects = Map(),
        bagObjects = Map(
          bagRoot.asLocation("data/README.txt") -> "This is a file\n",
          bagRoot.asLocation("data/ANOTHER.txt") -> "This is another file\n",
          bagRoot.asLocation("bag-info.txt") -> (
            "Bagging-Date: 2021-07-16\n" +
              "External-Identifier: mismatched_files\n" +
              "Payload-Oxum: 35.2\n"
          ),
          bagRoot.asLocation("bagit.txt") -> (
            "BagIt-Version: 0.97\n" +
              "Tag-File-Character-Encoding: UTF-8\n"
          ),
          bagRoot
            .asLocation("manifest-sha1.txt") -> "897589b7c274b17a1d02a74cf0b1128ad286d94e  data/ANOTHER.txt\n",
          bagRoot
            .asLocation("manifest-md5.txt") -> "a86e2699931d4f3d1456e79383749e43  data/README.txt\n",
          bagRoot.asLocation("tagmanifest-sha1.txt") -> (
            "7c3b03a943e9c9311f1b63348359fc603235a367  bag-info.txt\n" +
              "e2924b081506bac23f5fffe650ad1848a1c8ac1d  bagit.txt\n" +
              "620cf3a28d891c8e27e3f8b79fb9b87eec0b9543  manifest-sha1.txt\n" +
              "e0f93804f40bbeae4c5440ce197d3856e1367d77  manifest-md5.txt\n"
          ),
          bagRoot.asLocation("tagmanifest-md5.txt") -> (
            "139536a64db2ac0373fcfd83a379718b  bag-info.txt\n" +
              "9e5ad981e0d29adc278f6a294b8c2aca  bagit.txt\n" +
              "a1e301444f5e48cebfb0480e6ded97ec  manifest-sha1.txt\n" +
              "7983626d0844789acfe8059b6730b9d1  manifest-md5.txt\n"
          )
        ),
        bagRoot = bagRoot,
        bagInfo = bagInfo
      )

      storeBagContents(bagContents)

      val err = withBagReader {
        _.get(bagRoot).left.value
      }

      err.msg shouldBe "Payload manifests are inconsistent: every payload file must be listed in every payload manifest"
    }
  }

  it("errors if there is no tag manifest") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      deleteFile(bagRoot, "tagmanifest-sha256.txt")

      withBagReader {
        _.get(bagRoot).left.value.msg shouldBe "Could not find any tag manifests in the bag"
      }
    }
  }

  it("errors if the tag manifest is malformed") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      scrambleFile(bagRoot, path = "tagmanifest-sha256.txt")

      withBagReader {
        _.get(bagRoot).left.value.msg should startWith(
          "Error loading tagmanifest-sha256.txt"
        )
      }
    }
  }

  it("errors if the tag manifests are inconsistent") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val space = createStorageSpace
      val externalIdentifier = ExternalIdentifier("multiple_manifests")
      val version = createBagVersion

      val bagRoot: BagPrefix =
        createBagRoot(space, externalIdentifier, version)(namespace)

      val bagInfo = BagInfo(
        payloadOxum = PayloadOxum(payloadBytes = 15, numberOfPayloadFiles = 1),
        externalIdentifier = externalIdentifier,
        baggingDate = LocalDate.now()
      )

      val bagContents = BagContents(
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
              "bfbd969850673f65d14917bcbe42e86df867e4e383702a4471eb0776f2f1cfa48ec102489416741dcf278344bc0229ac2a9011080ffe2a4e55a64540ed0291d9  manifest-sha512.txt\n"
          ),
          bagRoot.asLocation("tagmanifest-md5.txt") -> (
            "aa3c5e977224a9186dbb36ef1193be0d  bag-info.txt\n" +
              "9e5ad981e0d29adc278f6a294b8c2aca  bagit.txt\n" +
              "7983626d0844789acfe8059b6730b9d1  manifest-md5.txt\n"
          )
        ),
        bagRoot = bagRoot,
        bagInfo = bagInfo
      )

      storeBagContents(bagContents)

      val err = withBagReader {
        _.get(bagRoot).left.value
      }

      err.msg shouldBe "Tag manifests are inconsistent: each tag manifest should list the same set of tag files"
    }
  }

  it("errors if the bag only uses weak algorithms") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val space = createStorageSpace
      val externalIdentifier = ExternalIdentifier("weak_algorithms")
      val version = createBagVersion

      val bagRoot: BagPrefix =
        createBagRoot(space, externalIdentifier, version)(namespace)

      val bagInfo = BagInfo(
        payloadOxum = PayloadOxum(payloadBytes = 15, numberOfPayloadFiles = 1),
        externalIdentifier = externalIdentifier,
        baggingDate = LocalDate.now()
      )

      val bagContents = BagContents(
        fetchObjects = Map(),
        bagObjects = Map(
          bagRoot.asLocation("data/README.txt") -> "This is a file\n",
          bagRoot.asLocation("bag-info.txt") -> (
            "Bagging-Date: 2021-07-16\n" +
              "External-Identifier: weak_algorithms\n" +
              "Payload-Oxum: 15.1\n"
          ),
          bagRoot.asLocation("bagit.txt") -> (
            "BagIt-Version: 0.97\n" +
              "Tag-File-Character-Encoding: UTF-8\n"
          ),
          bagRoot
            .asLocation("manifest-md5.txt") -> "a86e2699931d4f3d1456e79383749e43  data/README.txt\n",
          bagRoot.asLocation("tagmanifest-md5.txt") -> (
            "6fbebc61602595b34d381bd86bb65365  bag-info.txt\n" +
              "9e5ad981e0d29adc278f6a294b8c2aca  bagit.txt\n" +
              "d570da37be627c3955c165422e667245  manifest-sha512.txt\n" +
              "7983626d0844789acfe8059b6730b9d1  manifest-md5.txt\n"
          )
        ),
        bagRoot = bagRoot,
        bagInfo = bagInfo
      )

      storeBagContents(bagContents)

      val err = withBagReader {
        _.get(bagRoot).left.value
      }

      err.msg shouldBe "Payload manifests only use weak checksums: add a payload manifest using SHA-256 or SHA-512"
    }
  }

  it("errors if the payload and tag manifests use different checksums") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val space = createStorageSpace
      val externalIdentifier = ExternalIdentifier("different_checksums")
      val version = createBagVersion

      val bagRoot: BagPrefix =
        createBagRoot(space, externalIdentifier, version)(namespace)

      val bagInfo = BagInfo(
        payloadOxum = PayloadOxum(payloadBytes = 15, numberOfPayloadFiles = 1),
        externalIdentifier = externalIdentifier,
        baggingDate = LocalDate.now()
      )

      val bagContents = BagContents(
        fetchObjects = Map(),
        bagObjects = Map(
          bagRoot.asLocation("data/README.txt") -> "This is a file\n",
          bagRoot.asLocation("bag-info.txt") -> (
            "Bagging-Date: 2021-07-16\n" +
              "External-Identifier: different_checksums\n" +
              "Payload-Oxum: 15.1\n"
          ),
          bagRoot.asLocation("bagit.txt") -> (
            "BagIt-Version: 0.97\n" +
              "Tag-File-Character-Encoding: UTF-8\n"
          ),
          bagRoot
            .asLocation("manifest-sha512.txt") -> "7cd31c95fc5a40e5be7bf46e84df52c6d8d50e9003dfb7e3b85ac9c704b90a63ac220147645ff22d410166356133d241a7346e452c863601ce68b82d075031f8  data/README.txt\n",
          bagRoot.asLocation("tagmanifest-sha256.txt") -> (
            "1ad8750c4a30a82cff48049c6aa65d60dfef7d54bc7f5f54055d2c60a5bba851  bag-info.txt\n" +
              "e91f941be5973ff71f1dccbdd1a32d598881893a7f21be516aca743da38b1689  bagit.txt\n" +
              "33f48fd5df3bb188f874c033adab20e39b8e24c823e32fae99ee539317e8badf  manifest-sha512.txt\n"
          )
        ),
        bagRoot = bagRoot,
        bagInfo = bagInfo
      )

      storeBagContents(bagContents)

      val err = withBagReader {
        _.get(bagRoot).left.value
      }

      err.msg shouldBe "Manifests are inconsistent: tag manifests should use the same algorithms as the payload manifests in the bag"
    }
  }

  it("passes if the fetch.txt does not exist") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      deleteFile(bagRoot, path = "fetch.txt")

      withBagReader {
        _.get(bagRoot).value.fetch shouldBe None
      }
    }
  }

  it("errors if the fetch file is malformed") {
    withFixtures { fixtures =>
      implicit val (context, typedStore, namespace, bucket) = fixtures

      val (bagRoot, _) = createBag()(namespace, bucket)
      scrambleFile(bagRoot, path = "fetch.txt")

      withBagReader {
        _.get(bagRoot).left.value.msg should startWith(
          "Error loading fetch.txt"
        )
      }
    }
  }

  protected def toString(ns: Namespace): String = ns.toString

  private def createBag()(namespace: Namespace, bucket: Bucket)(
    implicit typedStore: TypedStore[BagLocation, String]
  ): (BagPrefix, BagInfo) = {
    val bagContents = createBagContentsWith()(namespace, bucket)

    storeBagContents(bagContents)

    (bagContents.bagRoot, bagContents.bagInfo)
  }
}
