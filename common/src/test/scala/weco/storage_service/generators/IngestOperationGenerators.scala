package weco.storage_service.generators
import weco.storage_service.storage.models.{
  IngestCompleted,
  IngestFailed,
  IngestStepSucceeded
}

trait IngestOperationGenerators extends StorageRandomGenerators {

  case class TestSummary(description: String) {
    override def toString: String = this.description
  }

  def createTestSummary() = TestSummary(randomAlphanumeric())

  def createOperationSuccess() = createOperationSuccessWith()

  def createOperationSuccessWith(summary: TestSummary = createTestSummary()) =
    IngestStepSucceeded(summary)

  def createOperationCompleted() = createOperationCompletedWith()

  def createOperationCompletedWith(summary: TestSummary = createTestSummary()) =
    IngestCompleted(summary)

  def createOperationFailure() = createIngestFailureWith()

  def createIngestFailureWith(
    summary: TestSummary = createTestSummary(),
    throwable: Throwable = new RuntimeException("error"),
    maybeFailureMessage: Option[String] = None
  ) =
    IngestFailed(summary, throwable, maybeFailureMessage)
}
