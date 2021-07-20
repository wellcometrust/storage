package weco.storage_service.checksum

case class Checksum(
  algorithm: ChecksumAlgorithm,
  value: ChecksumValue
) {
  override def toString: String =
    s"${algorithm.pathRepr}:$value"
}
