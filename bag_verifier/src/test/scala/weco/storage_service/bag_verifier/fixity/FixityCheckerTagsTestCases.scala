package weco.storage_service.bag_verifier.fixity

import org.mockito.Mockito
import org.mockito.Mockito.{times, verify}
import weco.storage_service.checksum._
import weco.storage.store.Readable
import weco.storage.streaming.InputStreamWithLength
import weco.storage.{Identified, Location, Prefix}

trait FixityCheckerTagsTestCases[BagLocation <: Location, BagPrefix <: Prefix[
  BagLocation
], Namespace, Context, StreamReaderImpl <: Readable[
  BagLocation,
  InputStreamWithLength
]] extends FixityCheckerTestCases[
      BagLocation,
      BagPrefix,
      Namespace,
      Context,
      StreamReaderImpl
    ] {

  val contentString = "HelloWorld"
  val checksumString = "68e109f0f40ca72a15e05cc22786f8e6"
  val checksum = Checksum(MD5, ChecksumValue(checksumString))

  def tagName(algorithm: ChecksumAlgorithm): String =
    algorithm match {
      case MD5    => "Content-MD5"
      case SHA1   => "Content-SHA1"
      case SHA256 => "Content-SHA256"
      case SHA512 => "Content-SHA512"
    }

  it("sets a tag on a successfully-verified object") {
    withContext { implicit context =>
      withNamespace { implicit namespace =>
        val location = createLocationWith(namespace)
        putString(location, contentString)

        val expectedFileFixity = createDataDirectoryFileFixityWith(
          location = location,
          checksum = checksum
        )

        withFixityChecker { fixityChecker =>
          fixityChecker.check(expectedFileFixity) shouldBe a[
            FileFixityCorrect[_]
          ]
          fixityChecker.tags.get(location).value shouldBe Identified(
            location,
            Map(
              tagName(checksum.algorithm) -> checksumString
            )
          )
        }
      }
    }
  }

  it("skips checking if there's a matching tag from a previous verification") {
    withContext { implicit context =>
      withNamespace { implicit namespace =>
        val location = createLocationWith(namespace)
        putString(location, contentString)

        val expectedFileFixity = createDataDirectoryFileFixityWith(
          location = location,
          checksum = checksum
        )

        withStreamReader { streamReader =>
          val spyReader: StreamReaderImpl = Mockito.spy(streamReader)

          withFixityChecker(spyReader) { fixityChecker =>
            fixityChecker.check(expectedFileFixity) shouldBe a[
              FileFixityCorrect[_]
            ]

            // Readable.get() should have been called to read the object so
            // it can be verified.
            verify(spyReader, times(1)).get(location)

            // It shouldn't be read a second time, because we see the tag written by
            // the previous verification.
            fixityChecker.check(expectedFileFixity) shouldBe a[
              FileFixityCorrect[_]
            ]
            verify(spyReader, times(1)).get(location)
          }
        }
      }
    }
  }

  it("errors if there's a mismatched tag from a previous verification") {
    withContext { implicit context =>
      withNamespace { implicit namespace =>
        val location = createLocationWith(namespace)
        putString(location, contentString)

        val expectedFileFixity = createDataDirectoryFileFixityWith(
          location = location,
          checksum = checksum
        )

        val badExpectedFixity = expectedFileFixity.copy(
          checksum = checksum.copy(
            value = randomChecksumValue
          )
        )

        withStreamReader { streamReader =>
          val spyReader: StreamReaderImpl = Mockito.spy(streamReader)

          withFixityChecker(spyReader) { fixityChecker =>
            fixityChecker.check(expectedFileFixity) shouldBe a[
              FileFixityCorrect[_]
            ]

            // Readable.get() should have been called to read the object so
            // it can be verified.
            verify(spyReader, times(1)).get(location)

            // It shouldn't be read a second time, because we see the tag written by
            // the previous verification.
            val result = fixityChecker.check(badExpectedFixity)
            result shouldBe a[FileFixityMismatch[_]]
            result
              .asInstanceOf[FileFixityMismatch[BagLocation]]
              .e
              .getMessage should startWith(
              "Cached verification tag doesn't match expected checksum"
            )
            verify(spyReader, times(1)).get(location)
          }
        }
      }
    }
  }

  it("errors if there's a matching tag but the size is wrong") {
    withContext { implicit context =>
      withNamespace { implicit namespace =>
        val location = createLocationWith(namespace)
        putString(location, contentString)

        val expectedFileFixity = createFetchFileFixityWith(
          location = location,
          checksum = checksum
        )

        val badExpectedFixity = expectedFileFixity.copy(
          length = Some(contentString.length + 1)
        )

        withStreamReader { streamReader =>
          val spyReadable: StreamReaderImpl = Mockito.spy(streamReader)

          withFixityChecker(spyReadable) { fixityChecker =>
            fixityChecker.check(expectedFileFixity) shouldBe a[
              FileFixityCorrect[_]
            ]

            // Readable.get() should have been called to read the object so
            // it can be verified.
            verify(spyReadable, times(1)).get(location)

            // It shouldn't be read a second time, because we see the tag written by
            // the previous verification.
            val result = fixityChecker.check(badExpectedFixity)
            result shouldBe a[FileFixityMismatch[_]]
            result
              .asInstanceOf[FileFixityMismatch[BagLocation]]
              .e
              .getMessage should startWith("Lengths do not match")
            verify(spyReadable, times(1)).get(location)
          }
        }
      }
    }
  }

  it("doesn't set a tag if the verification fails") {
    withContext { implicit context =>
      withNamespace { implicit namespace =>
        val location = createLocationWith(namespace)
        putString(location, contentString)

        val expectedFileFixity = createDataDirectoryFileFixityWith(
          location = location
        )

        withFixityChecker { fixityChecker =>
          fixityChecker.check(expectedFileFixity) shouldBe a[
            FileFixityMismatch[_]
          ]

          fixityChecker.tags.get(location).value shouldBe Identified(
            location,
            Map.empty
          )
        }
      }
    }
  }

  it("adds one tag per checksum algorithm") {
    val contentString = "HelloWorld"

    val allChecksums = Seq(
      Checksum(MD5, ChecksumValue("68e109f0f40ca72a15e05cc22786f8e6")),
      Checksum(
        SHA1,
        ChecksumValue("db8ac1c259eb89d4a131b253bacfca5f319d54f2")
      ),
      Checksum(
        SHA256,
        ChecksumValue(
          "872e4e50ce9990d8b041330c47c9ddd11bec6b503ae9386a99da8584e9bb12c4"
        )
      )
    )

    withContext { implicit context =>
      withNamespace { implicit namespace =>
        val location = createLocationWith(namespace)
        putString(location, contentString)

        withFixityChecker { fixityChecker =>
          allChecksums.foreach { checksum =>
            val expectedFileFixity = createDataDirectoryFileFixityWith(
              location = location,
              checksum = checksum
            )

            fixityChecker.check(expectedFileFixity) shouldBe a[
              FileFixityCorrect[_]
            ]
          }

          fixityChecker.tags.get(location).value shouldBe Identified(
            location,
            Map(
              tagName(MD5) -> "68e109f0f40ca72a15e05cc22786f8e6",
              tagName(SHA1) -> "db8ac1c259eb89d4a131b253bacfca5f319d54f2",
              tagName(SHA256) -> "872e4e50ce9990d8b041330c47c9ddd11bec6b503ae9386a99da8584e9bb12c4"
            )
          )
        }
      }
    }

  }
}
