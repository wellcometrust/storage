package weco.storage_service.bagit.services

import weco.storage_service.bagit.models.{
  Bag,
  BagFetchMetadata,
  BagManifest,
  BagPath,
  MatchedLocation
}
import weco.storage_service.checksum.Checksum

/** A bag can contain concrete files or refer to files stored elsewhere
  * in the fetch file.  This object takes a list of files referenced in
  * the manifest and the fetch entries (if any), and works out which
  * are files held outside the main bag.
  *
  */
object BagMatcher {

  def correlateFetchEntries(
    bag: Bag
  ): Either[Throwable, Seq[MatchedLocation]] =
    for {
      payloadMatchedLocations <- correlateFetchEntryToBagFile(
        manifest = bag.manifest,
        fetchEntries = bag.fetch match {
          case Some(fetchEntry) => fetchEntry.entries
          case None             => Map.empty
        }
      )

      // The fetch.txt should never refer to tag files
      tagMatchedLocations <- correlateFetchEntryToBagFile(
        manifest = bag.tagManifest,
        fetchEntries = Map.empty
      )
    } yield payloadMatchedLocations ++ tagMatchedLocations

  def correlateFetchEntryToBagFile(
    manifest: BagManifest,
    fetchEntries: Map[BagPath, BagFetchMetadata]
  ): Either[Throwable, Seq[MatchedLocation]] = {
    // First construct the list of matched locations -- for every file in the bag,
    // we either have a fetch.txt entry or we don't.
    val matchedLocations =
      manifest.entries
        .map {
          case (bagPath, checksumValue) =>
            MatchedLocation(
              bagPath = bagPath,
              checksum = Checksum(
                algorithm = manifest.checksumAlgorithm,
                value = checksumValue
              ),
              fetchMetadata = fetchEntries.get(bagPath)
            )
        }

    // We also need to check whether there are any fetch entries which don't appear in
    // the list of BagFiles (i.e., the manifest).
    //
    // If they are, we should throw an error.
    val manifestPaths = manifest.entries.keys.toSet
    val fetchPaths = fetchEntries.collect { case (bagPath, _) => bagPath }.toSet

    val unexpectedFetchPaths = fetchPaths.diff(manifestPaths)

    if (unexpectedFetchPaths.nonEmpty) {
      val pathString = unexpectedFetchPaths
        .map { _.value }
        .toList
        .sorted
        .mkString(", ")

      Left(
        new RuntimeException(
          s"fetch.txt refers to paths that aren't in the bag manifest: $pathString"
        )
      )
    } else {
      Right(matchedLocations.toSeq)
    }
  }
}
