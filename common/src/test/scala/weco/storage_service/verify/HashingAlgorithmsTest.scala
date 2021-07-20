package weco.storage_service.verify

import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers
import weco.storage_service.fixtures.ReflectionHelpers

class HashingAlgorithmsTest extends AnyFunSpec with Matchers with ReflectionHelpers {

  // This is meant to help us remember to update the list if/when we add
  // new algorithms.
  it("knows about all the algorithms") {
    HashingAlgorithms.algorithms.size shouldBe getSubclasses[HashingAlgorithm].size
  }

  it("gets the correct path name") {
    MD5.pathRepr shouldBe "md5"
    SHA512.pathRepr shouldBe "sha512"
  }
}
