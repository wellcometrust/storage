package weco.storage_service.checksum

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import org.scalatest.prop.TableDrivenPropertyChecks

class ChecksumValueTest
    extends AnyFunSpec
    with Matchers
    with TableDrivenPropertyChecks {
  val equalityTestCases = Table(
    ("value1", "value2"),
    ("abc123", "abc123"),
    ("ABC123", "abc123"),
    ("abc123", "ABC123")
  )

  it("treats two equivalent checksums as equal") {
    forAll(equalityTestCases) {
      case (value1, value2) =>
        val checksum1 = ChecksumValue(value1)
        val checksum2 = ChecksumValue(value2)

        checksum1 == checksum2 shouldBe true
        checksum1 != checksum2 shouldBe false
    }
  }

  val differingTestCases = Table(
    ("value1", "value2"),
    ("abc123", "def456"),
    ("ABC123", "123ABC")
  )

  it("treats two different checksums as not-equal") {
    forAll(differingTestCases) {
      case (value1, value2) =>
        val checksum1 = ChecksumValue(value1)
        val checksum2 = ChecksumValue(value2)

        checksum1 == checksum2 shouldBe false
        checksum1 != checksum2 shouldBe true
    }
  }

  it("is not equal to a different type") {
    ChecksumValue("123") == "123" shouldBe false
  }
}
