package weco.storage_service.bagit.models

import java.time.LocalDate

case class BagInfo(
  externalIdentifier: ExternalIdentifier,
  payloadOxum: PayloadOxum,
  baggingDate: LocalDate,
  sourceOrganisation: Option[SourceOrganisation] = None,
  externalDescription: Option[ExternalDescription] = None,
  internalSenderIdentifier: Option[InternalSenderIdentifier] = None,
  internalSenderDescription: Option[InternalSenderDescription] = None
)

case class InternalSenderIdentifier(underlying: String) extends AnyVal {
  override def toString: String = underlying
}

case class InternalSenderDescription(underlying: String) extends AnyVal {
  override def toString: String = underlying
}

case class ExternalDescription(underlying: String) extends AnyVal {
  override def toString: String = underlying
}

case class SourceOrganisation(underlying: String) extends AnyVal {
  override def toString: String = underlying
}

case class PayloadOxum(payloadBytes: Long, numberOfPayloadFiles: Int) {
  override def toString = s"$payloadBytes.$numberOfPayloadFiles"
}
