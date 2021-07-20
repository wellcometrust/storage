package weco.storage_service.checksum

import org.apache.commons.codec.digest.MessageDigestAlgorithms

/** The list of algorithms here is based on the list in RFC 8493 § 2.4:
  * https://datatracker.ietf.org/doc/html/rfc8493#section-2.4
  *
  * It is not an exhaustive list, and we may add new checksums in the future.
  *
  * As much as possible, we try to keep all the code related to checksum algorithms
  * within this directory, to make it as easy as possible to add new algorithms later.
  *
  */
sealed trait ChecksumAlgorithm {
  val value: String

  // Quoting RFC 8493 § 2.4:
  //
  //    The name of the checksum algorithm MUST be normalized for use in the
  //    manifest's filename by lowercasing the common name of the algorithm
  //    and removing all non-alphanumeric characters.
  //
  def pathRepr: String = value.toLowerCase.replaceAll("[^a-z0-9]", "")

  // Quoting RFC 8493 § 2.4:
  //
  //    Starting with BagIt 1.0, bag creation and validation tools MUST
  //    support the SHA-256 and SHA-512 algorithms [RFC6234] and SHOULD
  //    enable SHA-512 by default when creating new bags.  For backwards
  //    compatibility, implementers SHOULD support MD5 [RFC1321] and SHA-1
  //    [RFC3174].
  //
  val isForBackwardsCompatibilityOnly: Boolean = false

  override def toString: String = value
}

case object ChecksumAlgorithms {
  val algorithms: Seq[ChecksumAlgorithm] = Seq(SHA512, SHA256, SHA1, MD5)
}

case object SHA512 extends ChecksumAlgorithm {
  val value: String = MessageDigestAlgorithms.SHA_512
}

case object SHA256 extends ChecksumAlgorithm {
  val value: String = MessageDigestAlgorithms.SHA_256
}

case object SHA1 extends ChecksumAlgorithm {
  val value: String = MessageDigestAlgorithms.SHA_1

  override val isForBackwardsCompatibilityOnly: Boolean = true
}

case object MD5 extends ChecksumAlgorithm {
  val value: String = MessageDigestAlgorithms.MD5

  override val isForBackwardsCompatibilityOnly: Boolean = true
}
