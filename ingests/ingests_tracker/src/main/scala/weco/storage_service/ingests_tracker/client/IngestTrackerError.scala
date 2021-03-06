package weco.storage_service.ingests_tracker.client

import weco.storage_service.ingests.models.{Ingest, IngestID, IngestUpdate}

sealed trait IngestTrackerError

sealed trait IngestTrackerCreateError extends IngestTrackerError {
  val ingest: Ingest
}

case class IngestTrackerCreateConflictError(ingest: Ingest)
    extends IngestTrackerCreateError

case class IngestTrackerUnknownCreateError(ingest: Ingest, err: Throwable)
    extends IngestTrackerCreateError

sealed trait IngestTrackerUpdateError extends IngestTrackerError {
  val ingestUpdate: IngestUpdate
}

case class IngestTrackerUpdateNonExistentIngestError(ingestUpdate: IngestUpdate)
    extends IngestTrackerUpdateError

case class IngestTrackerUpdateConflictError(ingestUpdate: IngestUpdate)
    extends IngestTrackerUpdateError

case class IngestTrackerUnknownUpdateError(
  ingestUpdate: IngestUpdate,
  err: Throwable
) extends IngestTrackerUpdateError

sealed trait IngestTrackerGetError extends IngestTrackerError {
  val id: IngestID
}

case class IngestTrackerNotFoundError(id: IngestID)
    extends IngestTrackerGetError

case class IngestTrackerUnknownGetError(id: IngestID, err: Throwable)
    extends IngestTrackerGetError
