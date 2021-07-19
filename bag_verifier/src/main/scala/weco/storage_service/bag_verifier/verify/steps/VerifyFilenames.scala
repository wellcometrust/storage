package weco.storage_service.bag_verifier.verify.steps

import weco.storage_service.bag_verifier.models.BagVerifierError
import weco.storage_service.bagit.models.{NewPayloadManifest, NewTagManifest}

trait VerifyFilenames {
  def verifyAllowedFilenames(
    filenames: Seq[String]
  ): Either[BagVerifierError, Unit] = {
    // Azure blob storage does not support blob names that end with a `.`
    // See https://docs.microsoft.com/en-us/rest/api/storageservices/naming-and-referencing-containers--blobs--and-metadata#resource-names
    val endsWithADot = filenames.filter { _.endsWith(".") }

    // Combine the results into a single message that can be logged.
    val results = Map(
      "Filenames cannot end with a ." -> endsWithADot
    )

    val logMessage = results
      .map {
        case (reason, badFilenames) =>
          if (badFilenames.isEmpty) {
            ""
          } else {
            s"$reason: ${badFilenames.mkString(", ")}"
          }
      }
      .filterNot { _.isEmpty }
      .mkString("; ")

    logMessage match {
      case ""      => Right(())
      case message => Left(BagVerifierError(message))
    }
  }

  // The BagIt spec says that payload files should exclusively be stored
  // in the "data/" directory.
  //
  // See https://tools.ietf.org/html/rfc8493#section-2.1.2
  def verifyPayloadFilenames(
    manifest: NewPayloadManifest
  ): Either[BagVerifierError, Unit] = {
    val paths = manifest.entries.map { case (path, _) => path.value }
    val badPaths = paths.filterNot { _.startsWith("data/") }

    if (badPaths.isEmpty) {
      Right(())
    } else {
      Left(
        BagVerifierError(
          s"Not all payload files are in the data/ directory: ${badPaths.mkString(", ")}"
        )
      )
    }
  }

  // The BagIt spec does not make recommendations where tag files should be stored;
  // we require that all tag files are stored in the root of the bag.
  // e.g. you can't put a tag file in the "data/" directory.
  //
  // This ensures a clean separation of BagIt elements and payload files.
  //
  // The tag manifest lists all files *except* the "manifest-{algorithm}.txt" tag manifest
  // itself.  We have a separate check (VerifyNoUnreferencedFiles) that would warn us if
  // there's a tag manifest somewhere other than the root of the bag.
  def verifyTagFileFilenames(
    manifest: NewTagManifest
  ): Either[BagVerifierError, Unit] = {
    val paths = manifest.entries.map { case (path, _) => path.value }
    val badPaths = paths.filter { _.contains("/") }

    if (badPaths.isEmpty) {
      Right(())
    } else {
      Left(
        BagVerifierError(
          s"Not all tag files are in the root directory: ${badPaths.mkString(", ")}"
        )
      )
    }
  }
}
