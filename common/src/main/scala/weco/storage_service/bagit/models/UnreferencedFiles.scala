package weco.storage_service.bagit.models

import weco.storage_service.checksum.ChecksumAlgorithms

/** In a bag, we have payload files.  Those files are referred to by a payload
  * manifest file, which is in turn referred to by a tag manifest file.  But how
  * do we know the tag manifest file is there?
  *
  *      tagmanifest-sha256.txt
  *        tag manifest file
  *               |
  *          (references)
  *               |
  *               v
  *       manifest-sha256.txt
  *      payload manifest file
  *              |
  *         (references)
  *              |
  *              v
  *           b12345.jpg
  *          payload file
  *
  * There's nothing in a bag that refers to the tag manifest file.  This is okay --
  * the chain of checksums/references has to stop somewhere!
  *
  * This object records the filenames of all the tag manifests we might expect to see.
  *
  * We only ignore tag manifests for checksum algorithms that the storage service knows
  * how to use -- so we know it's reading and verifying all the checksums in these
  * manifests.  If we start seeing bags with other manifests, we should consider adding
  * support for those checksums.
  *
  */
object UnreferencedFiles {
  val tagManifestFiles: Seq[String] =
    ChecksumAlgorithms.algorithms.map { h =>
      s"tagmanifest-${h.pathRepr}.txt"
    }
}
