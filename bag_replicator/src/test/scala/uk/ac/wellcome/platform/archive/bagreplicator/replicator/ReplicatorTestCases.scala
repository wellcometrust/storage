package uk.ac.wellcome.platform.archive.bagreplicator.replicator

import org.scalatest.EitherValues
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import uk.ac.wellcome.fixtures.TestWith
import uk.ac.wellcome.platform.archive.bagreplicator.replicator.models.{ReplicationFailed, ReplicationRequest, ReplicationSucceeded}
import uk.ac.wellcome.platform.archive.common.fixtures.StorageRandomThings
import uk.ac.wellcome.storage.tags.Tags
import uk.ac.wellcome.storage.{Identified, ObjectLocation, ObjectLocationPrefix}

trait ReplicatorTestCases[SrcNamespace, DstNamespace]
  extends AnyFunSpec
    with Matchers
    with EitherValues
    with StorageRandomThings {
  def withSrcNamespace[R](testWith: TestWith[SrcNamespace, R]): R
  def withDstNamespace[R](testWith: TestWith[DstNamespace, R]): R

  def withReplicator[R](testWith: TestWith[Replicator, R]): R

  def createSrcLocationWith(srcNamespace: SrcNamespace): ObjectLocation
  def createDstLocationWith(dstNamespace: DstNamespace, path: String): ObjectLocation

  def createSrcPrefixWith(srcNamespace: SrcNamespace): ObjectLocationPrefix
  def createDstPrefixWith(dstNamespace: DstNamespace): ObjectLocationPrefix

  def putSrcObject(location: ObjectLocation, contents: String): Unit
  def putDstObject(location: ObjectLocation, contents: String): Unit

  def getDstObject(location: ObjectLocation): String

  val srcTags: Tags[ObjectLocation]
  val dstTags: Tags[ObjectLocation]

  it("replicates all the objects under a prefix") {
    withSrcNamespace { srcNamespace =>
      withDstNamespace { dstNamespace =>
        val locations = (1 to 5).map { _ =>
          createSrcLocationWith(srcNamespace)
        }

        val objects = locations.map { _ -> randomAlphanumeric }.toMap

        objects.foreach {
          case (loc, contents) => putSrcObject(loc, contents)
        }

        val srcPrefix = createSrcPrefixWith(srcNamespace)
        val dstPrefix = createDstPrefixWith(dstNamespace)

        val result = withReplicator {
          _.replicate(
            ingestId = createIngestID,
            request = ReplicationRequest(
              srcPrefix = srcPrefix,
              dstPrefix = dstPrefix
            )
          )
        }

        result shouldBe a[ReplicationSucceeded]
        result.summary.maybeEndTime.isDefined shouldBe true
      }
    }
  }

  it("fails if there are already different objects in the prefix") {
    withSrcNamespace { srcNamespace =>
      withDstNamespace { dstNamespace =>
        val locations = (1 to 5).map { _ =>
          createSrcLocationWith(srcNamespace)
        }

        val objects = locations.map { _ -> randomAlphanumeric }.toMap

        objects.foreach {
          case (loc, contents) => putSrcObject(loc, contents)
        }

        val srcPrefix = createSrcPrefixWith(srcNamespace)
        val dstPrefix = createDstPrefixWith(dstNamespace)

        // Write something to the first destination.  The replicator should realise
        // this object already exists, and refuse to overwrite it.
        val badContents = randomAlphanumeric

        val dstLocation = createDstLocationWith(
          dstNamespace = dstNamespace,
          path = locations.head.path.replace("src/", "dst/")
        )

        putDstObject(dstLocation, contents = badContents)

        val result = withReplicator {
          _.replicate(
            ingestId = createIngestID,
            request = ReplicationRequest(
              srcPrefix = srcPrefix,
              dstPrefix = dstPrefix
            )
          )
        }

        result shouldBe a[ReplicationFailed]

        getDstObject(dstLocation) shouldBe badContents
      }
    }
  }

  it("fails if the underlying replication has an error") {
    val srcPrefix = withSrcNamespace { createSrcPrefixWith }
    val dstPrefix = withDstNamespace { createDstPrefixWith }

    val result = withReplicator {
      _.replicate(
        ingestId = createIngestID,
        request = ReplicationRequest(
          srcPrefix = srcPrefix,
          dstPrefix = dstPrefix
        )
      )
    }

    result shouldBe a[ReplicationFailed]
    result.summary.maybeEndTime.isDefined shouldBe true

    result.asInstanceOf[ReplicationFailed]
  }

  // The verifier will write a Content-SHA256 checksum tag to objects when it
  // verifies them.  If an object is then replicated to a new location, any existing
  // verification tags should be removed.
  it("doesn't copy tags from the existing objects") {
    withSrcNamespace { srcNamespace =>
      withDstNamespace { dstNamespace =>
        val location = createSrcLocationWith(srcNamespace)

        putSrcObject(location, contents = randomAlphanumeric)
        srcTags.update(location) { existingTags =>
          Right(existingTags ++ Map("Content-SHA256" -> "abcdef"))
        }

        val request = ReplicationRequest(
          srcPrefix = createSrcPrefixWith(srcNamespace),
          dstPrefix = createDstPrefixWith(dstNamespace)
        )

        val result = withReplicator {
          _.replicate(
            ingestId = createIngestID,
            request = request
          )
        }

        result shouldBe a[ReplicationSucceeded]

        val dstLocation = createDstLocationWith(
          dstNamespace, path = location.path
        )

        dstTags.get(dstLocation).right.value shouldBe Identified(
          dstLocation,
          Map.empty
        )
      }
    }
  }
}
