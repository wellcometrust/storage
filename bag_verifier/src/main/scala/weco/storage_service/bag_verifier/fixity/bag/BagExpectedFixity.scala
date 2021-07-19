package weco.storage_service.bag_verifier.fixity.bag

import grizzled.slf4j.Logging
import weco.storage_service.bag_verifier.fixity.{
  CannotCreateExpectedFixity,
  DataDirectoryFileFixity,
  ExpectedFileFixity,
  ExpectedFixity,
  FetchFileFixity
}
import weco.storage_service.bag_verifier.storage.{Locatable, Resolvable}
import weco.storage_service.bag_verifier.storage.bag.BagLocatable
import weco.storage_service.bagit.models._
import weco.storage_service.bagit.services.BagMatcher
import weco.storage.{Location, Prefix}

class BagExpectedFixity[BagLocation <: Location, BagPrefix <: Prefix[
  BagLocation
]](root: BagPrefix)(
  implicit resolvable: Resolvable[BagLocation]
) extends ExpectedFixity[Bag]
    with Logging {

  import BagLocatable._
  import Locatable._

  implicit val locatable: Locatable[BagLocation, BagPrefix, BagPath] =
    bagPathLocatable[BagLocation, BagPrefix]

  override def create(
    bag: Bag
  ): Either[CannotCreateExpectedFixity, Seq[ExpectedFileFixity]] = {
    debug(s"Attempting to get the fixity info for $bag")

    BagMatcher.correlateFetchEntries(bag) match {
      case Left(error) =>
        debug(s"Left: $error")
        Left(combine(Seq(error)))
      case Right(matched) =>
        debug(s"Right: $matched")

        val matches = matched.map(getVerifiableLocation)

        val failures = matches collect { case Left(f)           => f }
        val successes = matches collect { case Right(locations) => locations }

        debug(s"Got ($successes, $failures)")

        Either.cond(failures.isEmpty, successes, combine(failures))
    }
  }

  private def getVerifiableLocation(
    matched: MatchedLocation
  ): Either[Throwable, ExpectedFileFixity] =
    matched match {
      case MatchedLocation(bagPath, multiChecksum, Some(fetchEntry)) =>
        Right(
          FetchFileFixity(
            uri = fetchEntry.uri,
            path = bagPath,
            multiChecksum = multiChecksum,
            length = fetchEntry.length
          )
        )

      case MatchedLocation(bagPath: BagPath, multiChecksum, None) =>
        bagPath.locateWith(root) match {
          case Left(e) => Left(CannotCreateExpectedFixity(e.msg))
          case Right(location) =>
            Right(
              DataDirectoryFileFixity(
                uri = resolvable.resolve(location),
                path = bagPath,
                multiChecksum = multiChecksum
              )
            )
        }
    }

  private def combine(errors: Seq[Throwable]) =
    CannotCreateExpectedFixity(errors.map(_.getMessage).mkString("\n"))
}
