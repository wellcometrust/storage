package uk.ac.wellcome.platform.archive.common.storage.services

import com.amazonaws.services.s3.AmazonS3
import com.amazonaws.services.s3.model.S3ObjectInputStream
import grizzled.slf4j.Logging
import uk.ac.wellcome.platform.archive.common.storage.{Resolvable, Streamable}
import uk.ac.wellcome.storage.ObjectLocation

import scala.util.Try

object S3StreamableInstances {

  implicit class ObjectLocationStreamable(location: ObjectLocation)(implicit s3Client: AmazonS3) extends Logging {
    def toInputStream: Try[Option[S3ObjectInputStream]] = Try {
      debug(s"Converting $location to InputStream")

      val bucketExists = s3Client.doesBucketExistV2(location.namespace)

      if(!bucketExists) {
        throw new RuntimeException("The specified bucket is not valid!")
      }

      val objectExists = s3Client.doesObjectExist(
        location.namespace,
        location.key
      )

      val result = if(objectExists) {
        Some(
          s3Client.getObject(
            location.namespace,
            location.key
          ).getObjectContent
        )
      } else {
        None
      }

      debug(s"Got: $result")

      result
    }
  }

  implicit class ResolvableStreamable[T](t: T)(implicit s3Client: AmazonS3, resolver: Resolvable[T]) extends Logging {
    def from(root: ObjectLocation) = {
      debug(s"Attempting to resolve Streamable $t")

      val streamable = new Streamable[T, S3ObjectInputStream] {
        override def stream(t: T): Try[Option[S3ObjectInputStream]] = {
          debug(s"Converting $t to InputStream")

          val resolved = resolver.resolve(root)(t)

          resolved.toInputStream
        }
      }

      streamable.stream(t)
    }
  }
}
