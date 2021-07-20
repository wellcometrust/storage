package weco.storage_service.checksum

import weco.json.{TypedString, TypedStringOps}

class ChecksumValue(val value: String) extends TypedString[ChecksumValue] {
  val underlying: String = value

  // Quoting BagIt spec § 2.1.3 "Payload Manifest":
  //
  //      The hex-encoded checksum MAY use uppercase and/or lowercase
  //      letters.
  //
  // Thus, we treat two checksums as equal if they're the same up to case.
  //
  // We don't normalise checksums because it means the checksums in the original
  // bag manifest and the storage service APIs will be identical.  e.g. if the user
  // provides a bag with uppercase checksums, the storage manifest served from
  // the bags API will use the same uppercase values.
  //
  override def equals(that: Any): Boolean =
    that match {
      case other: ChecksumValue => value.toLowerCase == other.value.toLowerCase
      case _                    => false
    }
}

object ChecksumValue extends TypedStringOps[ChecksumValue] {
  override def apply(value: String): ChecksumValue =
    new ChecksumValue(value)

  def create(raw: String): ChecksumValue =
    ChecksumValue(raw.trim)
}
