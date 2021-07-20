package weco.storage_service.bagit.services

import java.io.{BufferedReader, InputStream, InputStreamReader}

import weco.storage_service.bagit.models.BagPath
import weco.storage_service.checksum.ChecksumValue

import scala.util.matching.Regex
import scala.util.{Failure, Success, Try}

/** Each line in a manifest file _manifest-algorithm.txt_ is of the form
  *
  *     checksum filepath
  *
  * where _checksum_ is a hex-encoded checksum for the file at _filepath_ created
  * using _algorithm_ (e.g. md5, sha256).  The payload manifest lists every
  * file in the data/ directory; the tag manifest lists every file in the
  * top directory.
  *
  * This class parses the contents of a manifest file.
  *
  * See https://tools.ietf.org/html/rfc8493#section-2.1.3
  *     https://tools.ietf.org/html/rfc8493#section-2.2.1
  *
  */
object BagManifestParser {

  def parse(inputStream: InputStream): Try[Map[BagPath, ChecksumValue]] = {
    val bufferedReader = new BufferedReader(
      new InputStreamReader(inputStream)
    )

    // Read the lines one by one, either getting a (filepath -> checksum) association,
    // or returning the line if it doesn't parse correctly.
    val lines = Iterator
      .continually(bufferedReader.readLine())
      .takeWhile { _ != null }
      .filter { _.nonEmpty }
      .toList

    val parsedLines = lines.map { parseSingleLine }

    val unparseableLines = parsedLines.collect {
      case Left(line) => line
    }

    // The BagIt spec says a manifest cannot refer to the same file more than once;
    // for example, this is illegal (even though the checksums match):
    //
    //    abcdef  myfile.txt
    //    abcdef  myfile.txt
    //
    // This looks for duplicate paths in the manifest file.
    val paths = parsedLines
      .collect { case Right(assoc) => assoc }
      .flatMap { _.keys }

    val duplicatePaths: List[BagPath] = paths
      .groupBy { identity }
      .mapValues { _.size }
      .filter { case (_, count) => count > 1 }
      .map { case (bagPath, _) => bagPath }
      .toList

    val entries: Map[BagPath, ChecksumValue] = parsedLines
      .collect { case Right(assoc) => assoc }
      .foldLeft(Map[BagPath, ChecksumValue]()) { _ ++ _ }

    if (unparseableLines.nonEmpty) {
      Failure(
        new RuntimeException(
          s"Failed to parse the following lines: $unparseableLines"
        )
      )
    } else if (duplicatePaths.nonEmpty) {
      Failure(
        new RuntimeException(
          s"Manifest contains duplicate paths: $duplicatePaths"
        )
      )
    } else {
      Success(entries)
    }
  }

  // Represents a single line in a manifest file, e.g.
  //
  //    3532b221585eaa6d2520068ec1938928a1069a72fc50a8df9f3472da3f458037  data/b12345678.xml
  //
  // Note: checksums can be upper or lowercase.
  //
  private val LINE_REGEX: Regex = """([0-9a-fA-F]+?)\s+(.+)""".r

  private def parseSingleLine(
    line: String
  ): Either[String, Map[BagPath, ChecksumValue]] = line match {
    case LINE_REGEX(checksum, filepath) =>
      Right(Map(BagPath.create(filepath) -> ChecksumValue.create(checksum)))
    case _ => Left(line)
  }
}
