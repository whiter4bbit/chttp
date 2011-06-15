package info.whiter4bbit.chttp

import java.net.{URL, HttpURLConnection}
import java.io.InputStream
import scalaz._
import Scalaz._

case class Bundle(conn: HttpURLConnection)
case class ResponseResult[R](result: R)

abstract class Processor[R] extends ((Bundle) => (Validation[Exception, R]))

trait BundleProcessor extends Processor[Bundle] { self: BundleProcessor =>
   def <<[R](processor: Processor[Bundle]) = new BundleProcessor {
      def apply(bundle: Bundle): Validation[Exception, Bundle] = {
         self(bundle).flatMap(processor(_))
      }
   }
   def >>[R](processor: Processor[ResponseResult[R]]) = new ResponseResultProcessor[R] {
      def apply(bundle: Bundle): Validation[Exception, ResponseResult[R]] = {
         self(bundle).flatMap(processor(_))
      }
   }
}

class RequestMethodProcessor(val method: String) extends BundleProcessor {
   def apply(bundle: Bundle): Validation[Exception, Bundle] = {
      bundle.conn.setRequestMethod(method)
      bundle.success
   }
}

class HeaderProcessor(val key: String, val value: String) extends BundleProcessor {
   def apply(bundle: Bundle): Validation[Exception, Bundle] = {
      bundle.conn.setRequestProperty(key, value)
      bundle.success
   }
}

abstract class ResponseResultProcessor[R] extends Processor[ResponseResult[R]] { self =>
   def !! [D](f: R => D) = new ResponseResultProcessor[D] {   
      def apply(bundle: Bundle): Validation[Exception, ResponseResult[D]] = {      
          self(bundle).flatMap((r: ResponseResult[R]) => ResponseResult(f(r.result)).success)
      }
   }
}

class ToStringProcessor extends ResponseResultProcessor[String] {
   def apply(bundle: Bundle): Validation[Exception, ResponseResult[String]] = {
      var str = ""
      try {
         val stream = bundle.conn.getInputStream
         str = scala.io.Source.fromInputStream(stream).mkString
	 ResponseResult(str).success
      } catch {
         case e: Exception => e.fail
      }
   }
}

object Shortcuts {
   def get = new RequestMethodProcessor("GET")
   def post = new RequestMethodProcessor("POST")
   def header(key: String, value: String) = new HeaderProcessor(key, value)
   def as_string = new ToStringProcessor
}

class HTTP(val _url: String) {
   def apply[R](processor: Processor[R]): Validation[Exception, R] = {
      val url = new URL(_url)
      val connection = url.openConnection.asInstanceOf[HttpURLConnection]
      processor(Bundle(connection)) 
   }
}
