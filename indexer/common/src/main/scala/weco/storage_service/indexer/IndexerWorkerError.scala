package weco.storage_service.indexer

sealed trait IndexerWorkerError extends Exception

case class RetryableIndexingError[T](payload: T, cause: Throwable)
    extends IndexerWorkerError {
  initCause(cause)
}

case class FatalIndexingError[T](payload: T) extends IndexerWorkerError
