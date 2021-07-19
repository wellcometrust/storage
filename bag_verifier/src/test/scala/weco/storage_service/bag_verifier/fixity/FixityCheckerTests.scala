package weco.storage_service.bag_verifier.fixity

import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage._
import weco.storage.generators.{MemoryLocationGenerators, StreamGenerators}
import weco.storage.providers.memory.{MemoryLocation, MemoryLocationPrefix}
import weco.storage.services.SizeFinder
import weco.storage.store.memory.{MemoryStore, MemoryStreamStore}
import weco.storage.streaming.Codec.stringCodec
import weco.storage.streaming.{Codec, InputStreamWithLength}
import weco.storage.tags.memory.MemoryTags
import weco.storage_service.bag_verifier.fixity.memory.MemoryFixityChecker
import weco.storage_service.bag_verifier.generators.FixityGenerators
import weco.storage_service.bag_verifier.storage.{
  Locatable,
  LocateFailure,
  LocationParsingError
}
import weco.storage_service.bagit.models.MultiChecksumValue
import weco.storage_service.verify.ChecksumValue

import java.io.FilterInputStream
import java.net.URI

class FixityCheckerTests
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with FixityGenerators[MemoryLocation]
    with MemoryLocationGenerators
    with StreamGenerators {
  override def resolve(location: MemoryLocation): URI =
    new URI(s"mem://$location")

  override def createLocation: MemoryLocation =
    createMemoryLocation

  describe("handles errors correctly") {
    it("turns an error in locate() into a FileFixityCouldNotRead") {
      val streamStore = MemoryStreamStore[MemoryLocation]()
      val tags = createMemoryTags

      val brokenChecker = new MemoryFixityChecker(streamStore, tags) {
        override val locator =
          new Locatable[MemoryLocation, MemoryLocationPrefix, URI] {
            def locate(
              uri: URI
            )(
              maybeRoot: Option[MemoryLocationPrefix]
            ): Either[LocateFailure[URI], MemoryLocation] =
              Left(LocationParsingError(uri, msg = "BOOM!"))
          }
      }

      val expectedFileFixity = createExpectedFileFixity
      brokenChecker.check(expectedFileFixity) shouldBe a[FileFixityCouldNotRead[
        _
      ]]
    }

    it("handles an error when trying to checksum the object") {
      val badStream = new FilterInputStream(createInputStream()) {
        override def read(b: Array[Byte], off: Int, len: Int): Int =
          throw new Throwable("BOOM!")
      }

      val closedStream = new InputStreamWithLength(
        badStream,
        length = randomInt(from = 1, to = 10)
      )

      closedStream.close()

      val streamStore = new MemoryStreamStore[MemoryLocation](
        memoryStore = new MemoryStore[MemoryLocation, Array[Byte]](
          initialEntries = Map.empty
        )
      ) {
        override def get(location: MemoryLocation): this.ReadEither =
          Right(Identified(location, closedStream))
      }

      val expectedFileFixity = createDataDirectoryFileFixity

      val tags = createMemoryTags

      val checker = new MemoryFixityChecker(streamStore, tags) {
        override protected val sizeFinder: SizeFinder[MemoryLocation] =
          new SizeFinder[MemoryLocation] {
            override def get(location: MemoryLocation): ReadEither =
              Right(Identified(location, closedStream.length))
          }
      }

      checker.check(expectedFileFixity) shouldBe a[
        FileFixityCouldNotGetChecksum[_]
      ]
    }

    it("if it can't write the fixity tags") {
      val streamStore = MemoryStreamStore[MemoryLocation]()

      val tags = new MemoryTags[MemoryLocation](initialTags = Map.empty) {
        override def get(
          location: MemoryLocation
        ): Either[ReadError, Identified[MemoryLocation, Map[String, String]]] =
          super.get(location) match {
            case Right(t) => Right(t)
            case Left(_: DoesNotExistError) =>
              Right(Identified(location, Map[String, String]()))
            case Left(err) => Left(err)
          }

        override protected def writeTags(
          id: MemoryLocation,
          tags: Map[String, String]
        ): Either[WriteError, Map[String, String]] = {
          Left(
            StoreWriteError(new Throwable("BOOM!"))
          )
        }
      }

      val contentString = "HelloWorld"
      val multiChecksum = MultiChecksumValue(
        sha256 = Some(
          ChecksumValue(
            "872e4e50ce9990d8b041330c47c9ddd11bec6b503ae9386a99da8584e9bb12c4"
          )
        )
      )

      val location = createMemoryLocation

      val inputStream = stringCodec.toStream(contentString).value
      streamStore.put(location)(inputStream) shouldBe a[Right[_, _]]

      val expectedFileFixity = createDataDirectoryFileFixityWith(
        location = location,
        multiChecksum = multiChecksum
      )

      val checker = new MemoryFixityChecker(streamStore, tags) {
        override protected val sizeFinder: SizeFinder[MemoryLocation] =
          new SizeFinder[MemoryLocation] {
            override def get(location: MemoryLocation): ReadEither =
              Right(Identified(location, contentString.length))
          }
      }

      checker.check(expectedFileFixity) shouldBe a[FileFixityCouldNotWriteTag[
        _
      ]]
    }
  }

  describe("it closes the InputStream when it's done reading") {
    it("if the checksum is correct") {
      val contentString = "HelloWorld"
      // sha256("HelloWorld")
      val multiChecksum = MultiChecksumValue(
        sha256 = Some(
          ChecksumValue(
            "872e4e50ce9990d8b041330c47c9ddd11bec6b503ae9386a99da8584e9bb12c4"
          )
        )
      )

      var isClosed: Boolean = false

      val inputStream: InputStreamWithLength = new InputStreamWithLength(
        Codec.stringCodec.toStream(contentString).right.get,
        length = contentString.length
      ) {
        override def close(): Unit = {
          isClosed = true
          super.close()
        }
      }

      val streamStore = new MemoryStreamStore[MemoryLocation](
        memoryStore = new MemoryStore[MemoryLocation, Array[Byte]](
          initialEntries = Map.empty
        )
      ) {
        override def get(location: MemoryLocation): ReadEither =
          Right(Identified(location, inputStream))
      }

      val expectedFileFixity =
        createDataDirectoryFileFixityWith(multiChecksum = multiChecksum)

      val tags = createMemoryTags

      val checker = new MemoryFixityChecker(streamStore, tags) {
        override protected val sizeFinder: SizeFinder[MemoryLocation] =
          new SizeFinder[MemoryLocation] {
            override def get(location: MemoryLocation): ReadEither =
              Right(Identified(location, inputStream.length))
          }
      }

      checker.check(expectedFileFixity) shouldBe a[FileFixityCorrect[_]]

      isClosed shouldBe true
    }

    it("if the checksum is incorrect") {
      var isClosed: Boolean = false

      val inputStream: InputStreamWithLength = new InputStreamWithLength(
        createInputStream(),
        length = randomInt(from = 1, to = 50)
      ) {
        override def close(): Unit = {
          isClosed = true
          super.close()
        }
      }

      val streamStore = new MemoryStreamStore[MemoryLocation](
        memoryStore = new MemoryStore[MemoryLocation, Array[Byte]](
          initialEntries = Map.empty
        )
      ) {
        override def get(location: MemoryLocation): ReadEither =
          Right(Identified(location, inputStream))
      }

      val expectedFileFixity = createExpectedFileFixity

      val tags = createMemoryTags

      val checker = new MemoryFixityChecker(streamStore, tags) {
        override protected val sizeFinder: SizeFinder[MemoryLocation] =
          new SizeFinder[MemoryLocation] {
            override def get(location: MemoryLocation): ReadEither =
              Right(Identified(location, inputStream.length))
          }
      }

      checker.check(expectedFileFixity) shouldBe a[FileFixityMismatch[_]]

      isClosed shouldBe true
    }
  }

  def createMemoryTags: MemoryTags[MemoryLocation] =
    new MemoryTags[MemoryLocation](initialTags = Map.empty) {
      override def get(
        location: MemoryLocation
      ): Either[ReadError, Identified[MemoryLocation, Map[String, String]]] =
        super.get(location) match {
          case Right(tags) => Right(tags)
          case Left(_: DoesNotExistError) =>
            Right(Identified(location, Map[String, String]()))
          case Left(err) => Left(err)
        }
    }
}
