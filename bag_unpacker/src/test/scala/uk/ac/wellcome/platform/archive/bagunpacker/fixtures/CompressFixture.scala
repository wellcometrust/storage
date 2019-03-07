package uk.ac.wellcome.platform.archive.bagunpacker.fixtures

import java.io.{File, _}

import grizzled.slf4j.Logging
import org.apache.commons.compress.archivers.{
  ArchiveEntry,
  ArchiveOutputStream,
  ArchiveStreamFactory
}
import org.apache.commons.compress.compressors.{
  CompressorOutputStream,
  CompressorStreamFactory
}
import org.apache.commons.io.IOUtils
import uk.ac.wellcome.fixtures.TestWith
import uk.ac.wellcome.platform.archive.common.fixtures.RandomThings
import uk.ac.wellcome.storage.ObjectLocation
import uk.ac.wellcome.storage.fixtures.S3
import uk.ac.wellcome.storage.fixtures.S3.Bucket

trait CompressFixture extends RandomThings with S3 with Logging {

  val defaultFileCount = 10

  def withArchive[R](
                      bucket: Bucket,
                      archiveFile: File
                    )(testWith: TestWith[ObjectLocation, R]) = {

    val srcKey = archiveFile.getName
    s3Client.putObject(bucket.name, srcKey, archiveFile)

    val dstLocation = ObjectLocation(
      bucket.name,
      srcKey
    )
    testWith(dstLocation)
  }

  def createTgzArchiveWithRandomFiles(fileCount: Int = 10)=
    createTgzArchiveWithFiles(
      randomFilesInDirs(
        fileCount,
        fileCount / 4
      )
    )

  def createTgzArchiveWithFiles(files: List[File])=
    createArchiveWith(
      "tar",
      "gz",
      files
    )

  def createArchiveWithRandomFiles(
                                    archiverName: String,
                                    compressorName: String,
                                    fileCount: Int = 10
                                  ) =
    createArchiveWith(
      archiverName,
      compressorName,
      randomFilesInDirs(
        fileCount,
        fileCount / 4
      )
    )

  def createArchiveWith(
    archiverName: String,
    compressorName: String,
    files: List[File]
  ) = {
    val archiveFile = File.createTempFile(
      randomUUID.toString,
      ".test"
    )

    val fileOutputStream =
      new FileOutputStream(archiveFile)

    val archive = new Archive(
      archiverName,
      compressorName,
      fileOutputStream
    )

    val entries = files.map { file =>
      val entryName = relativeToTmpDir(file)
      println(s"Archiving ${file.getAbsolutePath} in ${entryName}")
      archive.addFile(
          file,
          entryName
        )
      } toSet

    archive.finish()
    fileOutputStream.close()

    (archiveFile, files, entries)
  }

  def relativeToTmpDir(file: File) = {
    val path = (new File(tmpDir).toURI)
      .relativize(file.toURI)
      .getPath

    s"./$path"
  }

  class Archive(
    archiverName: String,
    compressorName: String,
    outputStream: OutputStream
  ) {

    private val compress = compressor(compressorName)(_)
    private val pack = packer(archiverName)(_)

    private val compressorOutputStream =
      compress(outputStream)

    private val archiveOutputStream = pack(
      compressorOutputStream
    )

    def finish() = {
      archiveOutputStream.flush()
      archiveOutputStream.finish()
      compressorOutputStream.flush()
      compressorOutputStream.close()
    }

    def addFile(
      file: File,
      entryName: String
    ) = {
      synchronized {

        val entry = archiveOutputStream
          .createArchiveEntry(file, entryName)

        val fileInputStream = new BufferedInputStream(
          new FileInputStream(file)
        )

        archiveOutputStream.putArchiveEntry(entry)

        IOUtils.copy(
          fileInputStream,
          archiveOutputStream
        )

        archiveOutputStream.closeArchiveEntry()

        fileInputStream.close()

        entry
      }
    }

    private def compressor(
      compressorName: String
    )(
      outputStream: OutputStream
    ): CompressorOutputStream = {

      val compressorStreamFactory =
        new CompressorStreamFactory()

      val bufferedOutputStream =
        new BufferedOutputStream(outputStream)

      compressorStreamFactory
        .createCompressorOutputStream(
          compressorName,
          bufferedOutputStream
        )
    }

    private def packer(
      archiverName: String
    )(
      outputStream: OutputStream
    ): ArchiveOutputStream = {

      val archiveStreamFactory =
        new ArchiveStreamFactory()

      val bufferedOutputStream =
        new BufferedOutputStream(outputStream)

      archiveStreamFactory
        .createArchiveOutputStream(
          archiverName,
          bufferedOutputStream
        )

    }
  }
}

case class TestArchive(
  archiveFile: File,
  containedFiles: List[File],
  archiveEntries: Set[ArchiveEntry],
  location: ObjectLocation
)
