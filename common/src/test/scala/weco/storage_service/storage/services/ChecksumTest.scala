package weco.storage_service.storage.services

import org.scalatest.EitherValues
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.checksum.{Checksum, ChecksumValue, SHA256}
import weco.storage_service.generators.StorageRandomGenerators

class ChecksumTest
    extends AnyFunSpec
    with Matchers
    with EitherValues
    with ScalaFutures
    with StorageRandomGenerators {

  it("creates a useful string representation") {
    val checksum = Checksum(
      SHA256,
      ChecksumValue("1234567890")
    )

    checksum.toString shouldBe "sha256:1234567890"
  }
}
