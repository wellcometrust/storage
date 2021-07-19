package weco.storage_service.bag_verifier.fixity

import java.net.URI
import grizzled.slf4j.Logging
import weco.storage_service.bag_verifier.storage.Locatable._
import weco.storage_service.bag_verifier.storage.{
  Locatable,
  LocationError,
  LocationNotFound,
  LocationParsingError
}
import weco.storage_service.verify._
import weco.storage.services.SizeFinder
import weco.storage.store.Readable
import weco.storage.streaming.InputStreamWithLength
import weco.storage.tags.Tags
import weco.storage.{DoesNotExistError, Location, Prefix, ReadError}
import weco.storage_service.bagit.models.MultiChecksumValue

import scala.util.{Failure, Success}

/** Look up and check the fixity info (checksum, size) on an individual file.
  *
  */
trait FixityChecker[BagLocation <: Location, BagPrefix <: Prefix[BagLocation]]
    extends Logging {
  protected val streamReader: Readable[BagLocation, InputStreamWithLength]
  protected val sizeFinder: SizeFinder[BagLocation]
  val tags: Tags[BagLocation]
  implicit val locator: Locatable[BagLocation, BagPrefix, URI]

  def check(
    expectedFileFixity: ExpectedFileFixity
  ): FileFixityResult[BagLocation] = {
    debug(s"Attempting to verify: $expectedFileFixity")

    // The verifier writes tag to the storage location after it's verified.
    //
    // If it tries to verify a location and finds the correct tag, it skips
    // re-reading the entire file.
    //
    // This means re-verifying the same content (e.g. a new version of a bag that
    // refers back to files in an older version):
    //
    //    1.  Is faster than reading the entire object again
    //    2.  Works even if the objects have been cycled to Glacier/cold storage
    //        (e.g. if they're very large and infrequently accessed)

    val fixityResult = for {
      location <- parseLocation(expectedFileFixity)
      _ = debug(
        s"Parsed location for ${expectedFileFixity.uri} as $location"
      )

      existingTags <- getExistingTags(expectedFileFixity, location)
      _ = debug(s"Got existing tags for $location: $existingTags")

      size <- verifySize(expectedFileFixity, location)
      _ = debug(s"Checked the size of $location is correct")

      result <- verifyChecksum(
        expectedFileFixity = expectedFileFixity,
        location = location,
        existingTags = existingTags,
        size = size
      )

      _ <- writeFixityTags(expectedFileFixity, location)
    } yield result

    debug(s"Fixity check result for ${expectedFileFixity.uri}: $fixityResult")

    fixityResult match {
      case Right(fixityCorrect) => fixityCorrect
      case Left(fixityError)    => fixityError
    }
  }

  private def parseLocation(
    expectedFileFixity: ExpectedFileFixity
  ): Either[FileFixityCouldNotRead[BagLocation], BagLocation] =
    expectedFileFixity.uri.locate match {
      case Right(location) => Right(location)
      case Left(locateError) =>
        Left(
          FileFixityCouldNotRead(
            expectedFileFixity = expectedFileFixity,
            e = LocationParsingError(expectedFileFixity, locateError.msg)
          )
        )
    }

  private def getExistingTags(
    expectedFileFixity: ExpectedFileFixity,
    location: BagLocation
  ): Either[FileFixityCouldNotRead[BagLocation], Map[String, String]] =
    handleReadErrors(
      tags
        .get(location)
        .map { _.identifiedT },
      expectedFileFixity = expectedFileFixity
    )

  private def openInputStream(
    expectedFileFixity: ExpectedFileFixity,
    location: BagLocation
  ): Either[FileFixityCouldNotRead[BagLocation], InputStreamWithLength] =
    handleReadErrors(
      streamReader.get(location),
      expectedFileFixity = expectedFileFixity
    ).map { _.identifiedT }

  private def verifySize(
    expectedFileFixity: ExpectedFileFixity,
    location: BagLocation
  ): Either[FileFixityError[BagLocation], Long] =
    for {
      actualLength <- handleReadErrors(
        sizeFinder.getSize(location),
        expectedFileFixity = expectedFileFixity
      )

      _ <- expectedFileFixity.length match {
        case Some(expectedLength) if expectedLength != actualLength =>
          Left(
            FileFixityMismatch(
              expectedFileFixity = expectedFileFixity,
              objectLocation = location,
              e = new Throwable(
                s"Lengths do not match: $expectedLength (expected) != $actualLength (actual)"
              )
            )
          )

        case _ => Right(())
      }
    } yield actualLength

  private def verifyChecksum(
    expectedFileFixity: ExpectedFileFixity,
    location: BagLocation,
    existingTags: Map[String, String],
    size: Long
  ): Either[FileFixityError[BagLocation], FileFixityCorrect[BagLocation]] = {
    val algorithms = expectedFileFixity.multiChecksum.algorithms

    val expectedChecksumTags =
      algorithms.map { h =>
        fixityTagName(h) -> expectedFileFixity.multiChecksum
          .getValue(h)
          .get
          .value
      }.toMap

    val existingChecksumTags =
      existingTags.filter {
        case (key, _) => expectedChecksumTags.contains(key)
      }

    (expectedChecksumTags, existingChecksumTags) match {

      // Case 1: all the fixity tags on the object match the expected fixities,
      // so we know this object has already been verified.
      case (expected, actual) if actual == expected =>
        Right(
          FileFixityCorrect(
            expectedFileFixity = expectedFileFixity,
            objectLocation = location,
            size = size
          )
        )

      // Case 2: there's a fixity tag on the object which is different from the
      // value we expected.
      case (expected, actual) if actual.exists {
            case (key, value) => expected(key) != value
          } =>
        Left(
          FileFixityMismatch(
            expectedFileFixity = expectedFileFixity,
            objectLocation = location,
            e = new Throwable(
              s"Cached verification tag doesn't match expected checksum for $location: $actual (expected $expected)"
            )
          )
        )

      // Case 3: all other cases.  This includes the case where there are no verification
      // tags on the object, or there are tags for only some algorithms.
      case _ =>
        // Note: it is possible for something to go wrong *after* we open
        // the input stream, e.g. if the stream gets interrupted.
        //
        // This may be a retryable error -- e.g. a timeout -- and we could
        // do some retrying, rather than failing the entire ingest.
        //
        // In practice, those errors are extremely rare, and using the chunked
        // stream readers (see https://github.com/wellcomecollection/storage-service/pull/825)
        // should make them even rarer.  This is an obvious place to add more
        // retrying logic if it looks like we need it, but I'm not going to
        // futz with code that works almost perfectly already. ~ AWLC, 31 March 2021
        openInputStream(expectedFileFixity, location) match {
          case Left(err) => Left(err)
          case Right(inputStream) =>
            verifyChecksumFromInputStream(
              expectedFileFixity = expectedFileFixity,
              location = location,
              inputStream = inputStream,
              size = size
            )
        }
    }
  }

  private def verifyChecksumFromInputStream(
    expectedFileFixity: ExpectedFileFixity,
    location: BagLocation,
    inputStream: InputStreamWithLength,
    size: Long
  ): Either[FileFixityError[BagLocation], FileFixityCorrect[BagLocation]] = {
    // This assertion should never fire in practice -- if it does, it means
    // an object is changing under our feet.  If that's the case, there's something
    // badly wrong with the storage service.
    //
    // There's nothing sensible we can do to recover, so throw immediately and wait
    // for a human to come and inspect the issue.
    assert(
      size == inputStream.length,
      message =
        s"The size of $location has changed!  Before: $size, after: ${inputStream.length}"
    )

    val fixityResult = Hasher.hash(inputStream) match {
      case Failure(e) =>
        Left(
          FileFixityCouldNotGetChecksum(
            expectedFileFixity = expectedFileFixity,
            objectLocation = location,
            e = FailedChecksumCreation(e)
          )
        )

      case Success(hashingResult)
          if isMatch(hashingResult, expectedFileFixity.multiChecksum) =>
        Right(
          FileFixityCorrect(
            expectedFileFixity = expectedFileFixity,
            objectLocation = location,
            size = inputStream.length
          )
        )

      case Success(hashingResult) =>
        Left(
          FileFixityMismatch(
            expectedFileFixity = expectedFileFixity,
            objectLocation = location,
            e = FailedChecksumNoMatch(
              actual = hashingResult,
              expected = expectedFileFixity.multiChecksum
            )
          )
        )
    }

    // Remember to close the InputStream when we're done, whatever the result.
    // If we don't, the verifier will accumulate open streams and run out of
    // either memory or file descriptors.
    inputStream.close()

    fixityResult
  }

  private def isMatch(
    hashingResult: HashingResult,
    multiChecksum: MultiChecksumValue[ChecksumValue]
  ): Boolean =
    multiChecksum.algorithms
      .map { h =>
        val expectedValue = multiChecksum.getValue(h).get
        val actualValue = hashingResult.getChecksumValue(h)

        expectedValue == actualValue
      }
      .forall(_ == true)

  private def writeFixityTags(
    fileFixity: ExpectedFileFixity,
    location: BagLocation
  ): Either[FileFixityCouldNotWriteTag[BagLocation], Unit] =
    tags
      .update(location) { existingTags =>
        val fixityTags =
          fileFixity.multiChecksum.algorithms.map { h =>
            fixityTagName(h) -> fileFixity.multiChecksum.getValue(h).get.value
          }.toMap

        // We've already checked the tags on this location once, so we shouldn't
        // see conflicting values here.  Check we're not about to blat some existing
        // tags just in case.  If we do see conflicting tags here, there's something
        // badly wrong with the storage service.
        //
        // Note: this is a fairly weak guarantee, because tags aren't locked during
        // an update operation.
        existingTags.foreach {
          case (tagName, tagValue) =>
            fixityTags.get(tagName) match {
              case Some(value) =>
                assert(
                  value == tagValue,
                  s"Trying to write $fixityTags to $location; existing tags conflict: $existingTags"
                )
              case _ => ()
            }
        }

        Right(existingTags ++ fixityTags)
      } match {
      case Right(_) => Right(())
      case Left(writeError) =>
        Left(
          FileFixityCouldNotWriteTag(
            expectedFileFixity = fileFixity,
            objectLocation = location,
            e = writeError.e
          )
        )
    }

  // e.g. Content-MD5, Content-SHA256
  protected def fixityTagName(algorithm: HashingAlgorithm): String =
    s"Content-${algorithm.pathRepr.toUpperCase}"

  private def handleReadErrors[T](
    t: Either[ReadError, T],
    expectedFileFixity: ExpectedFileFixity
  ): Either[FileFixityCouldNotRead[BagLocation], T] =
    t match {
      case Right(value) => Right(value)

      case Left(_: DoesNotExistError) =>
        Left(
          FileFixityCouldNotRead(
            expectedFileFixity = expectedFileFixity,
            e = LocationNotFound(expectedFileFixity, "Location not available!")
          )
        )

      case Left(readError) =>
        Left(
          FileFixityCouldNotRead(
            expectedFileFixity = expectedFileFixity,
            e = LocationError(expectedFileFixity, readError.e.getMessage)
          )
        )
    }
}
