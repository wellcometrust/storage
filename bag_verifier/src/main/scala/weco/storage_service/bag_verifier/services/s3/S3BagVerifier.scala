package weco.storage_service.bag_verifier.services.s3

import com.amazonaws.services.s3.AmazonS3
import weco.storage_service.bag_verifier.fixity.FixityListChecker
import weco.storage_service.bag_verifier.fixity.s3.S3FixityChecker
import weco.storage_service.bag_verifier.models.{
  BagVerifierError,
  BagVerifyContext,
  ReplicatedBagVerifyContext,
  StandaloneBagVerifyContext
}
import weco.storage_service.bag_verifier.services.{
  BagVerifier,
  ReplicatedBagVerifier
}
import weco.storage_service.bag_verifier.storage.Resolvable
import weco.storage_service.bag_verifier.storage.s3.S3Resolvable
import weco.storage_service.bagit.models.{Bag, ExternalIdentifier}
import weco.storage_service.bagit.services.BagReader
import weco.storage_service.bagit.services.s3.S3BagReader
import weco.storage_service.storage.models.StorageSpace
import weco.storage.listing.Listing
import weco.storage.listing.s3.S3ObjectLocationListing
import weco.storage.s3.{S3ObjectLocation, S3ObjectLocationPrefix}
import weco.storage.store.StreamStore
import weco.storage.store.s3.S3StreamStore

trait S3BagVerifier[B <: BagVerifyContext[S3ObjectLocationPrefix]]
    extends BagVerifier[B, S3ObjectLocation, S3ObjectLocationPrefix] {

  override def getRelativePath(
    root: S3ObjectLocationPrefix,
    location: S3ObjectLocation
  ): String =
    location.key.replace(root.keyPrefix, "")
}

class S3StandaloneBagVerifier(
  val primaryBucket: String,
  val bagReader: BagReader[S3ObjectLocation, S3ObjectLocationPrefix],
  val listing: Listing[S3ObjectLocationPrefix, S3ObjectLocation],
  val resolvable: Resolvable[S3ObjectLocation],
  val fixityListChecker: FixityListChecker[
    S3ObjectLocation,
    S3ObjectLocationPrefix,
    Bag
  ],
  val streamReader: S3StreamStore
) extends BagVerifier[
      StandaloneBagVerifyContext,
      S3ObjectLocation,
      S3ObjectLocationPrefix
    ]
    with S3BagVerifier[StandaloneBagVerifyContext] {
  override def verifyReplicatedBag(
    context: StandaloneBagVerifyContext,
    space: StorageSpace,
    externalIdentifier: ExternalIdentifier,
    bag: Bag
  ): Either[BagVerifierError, Unit] = Right(())

}

class S3ReplicatedBagVerifier(
  val primaryBucket: String,
  val bagReader: BagReader[S3ObjectLocation, S3ObjectLocationPrefix],
  val listing: Listing[S3ObjectLocationPrefix, S3ObjectLocation],
  val resolvable: Resolvable[S3ObjectLocation],
  val fixityListChecker: FixityListChecker[
    S3ObjectLocation,
    S3ObjectLocationPrefix,
    Bag
  ],
  val srcReader: StreamStore[S3ObjectLocation],
  val streamReader: S3StreamStore
) extends ReplicatedBagVerifier[
      S3ObjectLocation,
      S3ObjectLocationPrefix
    ]
    with S3BagVerifier[
      ReplicatedBagVerifyContext[S3ObjectLocationPrefix]
    ]

object S3BagVerifier {
  def standalone(
    primaryBucket: String
  )(implicit s3Client: AmazonS3): S3StandaloneBagVerifier = {
    val bagReader = new S3BagReader()
    val listing = S3ObjectLocationListing()
    val resolvable = new S3Resolvable()
    implicit val fixityChecker = S3FixityChecker()
    val fixityListChecker =
      new FixityListChecker[S3ObjectLocation, S3ObjectLocationPrefix, Bag]()
    new S3StandaloneBagVerifier(
      primaryBucket,
      bagReader,
      listing,
      resolvable,
      fixityListChecker,
      streamReader = new S3StreamStore()
    )
  }
  def replicated(
    primaryBucket: String
  )(implicit s3Client: AmazonS3): S3ReplicatedBagVerifier = {
    val bagReader = new S3BagReader()
    val listing = S3ObjectLocationListing()
    val resolvable = new S3Resolvable()
    implicit val fixityChecker = S3FixityChecker()
    val fixityListChecker =
      new FixityListChecker[S3ObjectLocation, S3ObjectLocationPrefix, Bag]()
    val streamStore = new S3StreamStore()
    new S3ReplicatedBagVerifier(
      primaryBucket,
      bagReader,
      listing,
      resolvable,
      fixityListChecker,
      srcReader = streamStore,
      streamReader = streamStore
    )
  }
}
