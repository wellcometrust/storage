package weco.storage_service.checksum

import org.apache.commons.codec.binary.Hex

import java.io.InputStream
import java.security.MessageDigest
import scala.util.Try

/** This class records the actual checksum of a file in a bag, based on the
  * contents of the file as read from storage.  It records checksums for every
  * checksum algorithm supported by the storage service.
  *
  */
case class MultiChecksum(
  md5: ChecksumValue,
  sha1: ChecksumValue,
  sha256: ChecksumValue,
  sha512: ChecksumValue
) {
  def getValue(algorithm: ChecksumAlgorithm): ChecksumValue =
    algorithm match {
      case MD5    => md5
      case SHA1   => sha1
      case SHA256 => sha256
      case SHA512 => sha512
    }

  /** Compare to the expected checksum information.  Does the actual file match the
    * expected checksum, or are they different?
    *
    */
  def matches(manifestChecksum: MultiManifestChecksum): Boolean =
    manifestChecksum.definedAlgorithms
      .forall { a =>
        val expected = manifestChecksum.getValue(a)
        val actual = getValue(a)

        expected.contains(actual)
      }
}

case object MultiChecksum {
  def create(inputStream: InputStream): Try[MultiChecksum] = Try {
    val digest_MD5: MessageDigest = MessageDigest.getInstance(MD5.value)
    val digest_SHA1 = MessageDigest.getInstance(SHA1.value)
    val digest_SHA256 = MessageDigest.getInstance(SHA256.value)
    val digest_SHA512 = MessageDigest.getInstance(SHA512.value)

    // This implementation is based on MessageDigest.updateDigest(), but rather than
    // updating a single digest, we update all four at once.
    val STREAM_BUFFER_LENGTH = 1024

    val buffer = new Array[Byte](STREAM_BUFFER_LENGTH)
    var read = inputStream.read(buffer, 0, STREAM_BUFFER_LENGTH)

    while (read > -1) {
      digest_MD5.update(buffer, 0, read)
      digest_SHA1.update(buffer, 0, read)
      digest_SHA256.update(buffer, 0, read)
      digest_SHA512.update(buffer, 0, read)

      read = inputStream.read(buffer, 0, STREAM_BUFFER_LENGTH)
    }
    // == MessageDigest.updateDigest() ends ==

    MultiChecksum(
      md5 = asChecksumValue(digest_MD5),
      sha1 = asChecksumValue(digest_SHA1),
      sha256 = asChecksumValue(digest_SHA256),
      sha512 = asChecksumValue(digest_SHA512)
    )
  }

  private def asChecksumValue(digest: MessageDigest): ChecksumValue =
    ChecksumValue(Hex.encodeHexString(digest.digest))
}
