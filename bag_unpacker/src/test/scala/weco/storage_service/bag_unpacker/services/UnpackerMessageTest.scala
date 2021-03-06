package weco.storage_service.bag_unpacker.services

import java.time.Instant

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.bag_unpacker.models.UnpackSummary
import weco.storage_service.generators.StorageRandomGenerators
import weco.storage.generators.MemoryLocationGenerators

import scala.util.Random

class UnpackerMessageTest
    extends AnyFunSpec
    with Matchers
    with MemoryLocationGenerators
    with StorageRandomGenerators {
  it("handles a single file correctly") {
    val summary = createSummaryWith(fileCount = 1)

    UnpackerMessage.create(summary) should endWith("from 1 file")
  }

  it("handles multiple files correctly") {
    val summary = createSummaryWith(fileCount = 5)

    UnpackerMessage.create(summary) should endWith("from 5 files")
  }

  it("adds a comma to the file counts if appropriate") {
    val summary = createSummaryWith(fileCount = 123456789)

    UnpackerMessage.create(summary) should endWith("from 123,456,789 files")
  }

  it("pretty-prints the file size") {
    val summary = createSummaryWith(bytesUnpacked = 123456789)

    UnpackerMessage.create(summary) should startWith("Unpacked 117 MB")
  }

  def createSummaryWith(
    fileCount: Long = Random.nextLong(),
    bytesUnpacked: Long = Random.nextLong()
  ): UnpackSummary[_, _] =
    UnpackSummary(
      ingestId = createIngestID,
      srcLocation = createMemoryLocation,
      dstPrefix = createMemoryLocationPrefix,
      fileCount = fileCount,
      bytesUnpacked = bytesUnpacked,
      startTime = Instant.now()
    )
}
