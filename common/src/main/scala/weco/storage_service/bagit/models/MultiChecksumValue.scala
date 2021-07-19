package weco.storage_service.bagit.models

import weco.storage_service.verify.{HashingAlgorithm, MD5, SHA1, SHA256, SHA512}

sealed trait MultiChecksumException extends RuntimeException

object MultiChecksumException {
  case object NoChecksum extends MultiChecksumException
  case object OnlyWeakChecksums extends MultiChecksumException
}

case class MultiChecksumValue[T](
  md5: Option[T] = None,
  sha1: Option[T] = None,
  sha256: Option[T] = None,
  sha512: Option[T] = None
) {
  if (md5.isEmpty && sha1.isEmpty && sha256.isEmpty && sha512.isEmpty) {
    throw MultiChecksumException.NoChecksum
  }

  // We support MD5 and SHA1 for backwards compatibility (see RFC 8493 § 2.4)
  // but we require that all new bags use at least one of SHA-256 or SHA-512.
  if (sha256.isEmpty && sha512.isEmpty) {
    throw MultiChecksumException.OnlyWeakChecksums
  }

  def algorithms: Seq[HashingAlgorithm] =
    Seq(
      md5.map(_ => MD5),
      sha1.map(_ => SHA1),
      sha256.map(_ => SHA256),
      sha512.map(_ => SHA512)
    ).flatten

  def getValue(h: HashingAlgorithm): Option[T] =
    h match {
      case SHA512 => sha512
      case SHA256 => sha256
      case SHA1   => sha1
      case MD5    => md5
    }
}
