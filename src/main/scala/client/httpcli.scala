package info.whiter4bbit.chttp

import java.net.{URL, HttpURLConnection}
import java.io.InputStream
import scalaz._
import Scalaz._

case class Bundle(conn: HttpURLConnection, val params: Map[String, String])
case class ResponseResult[R](result: R)

abstract class Processor[R] extends ((Bundle) => (Validation[Exception, R]))

trait BundleProcessor extends Processor[Bundle] { self: BundleProcessor =>
   def <<[R](processor: Processor[Bundle]) = new BundleProcessor {
      def apply(bundle: Bundle): Validation[Exception, Bundle] = {
         self(bundle).flatMap(processor(_))
      }
   }
   def >>[R](processor: Processor[ResponseResult[R]]) = new ResponseResultProcessor[R] {
      import java.io.DataOutputStream     
      import java.net.URLDecoder

      def apply(bundle: Bundle): Validation[Exception, ResponseResult[R]] = {         
         self(bundle).flatMap((bundle) => { 
           if (bundle.params.size > 0 && bundle.conn.getRequestMethod.equals("POST")) {
	      bundle.conn.setDoOutput(true)
              val data = bundle.params.toList.map((p) => p._1 + "=" + URLDecoder.decode(p._2, "UTF-8")).mkString("&")
	      bundle.conn.setRequestProperty("Content-Type", "application/x-www-form-urlencoded")	 
	      bundle.conn.setRequestProperty("Content-Length", data.length.toString)
	      var os: DataOutputStream = null
	      try {
	        os = new DataOutputStream(bundle.conn.getOutputStream)
	        os.writeBytes(data)
	        os.flush
	      } catch {
	        case e: Exception => {
		  return e.fail
		}
	      } finally {
	        if (os != null) {
		   os.close
		}
	      }
	   }
	   return processor(bundle)
	 })
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

class ParamProcessor(val key: String, val value: String) extends BundleProcessor {
   def apply(bundle: Bundle): Validation[Exception, Bundle] = {
      Bundle(bundle.conn, bundle.params + (key -> value)).success
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
   def param(key: String, value: String) = new ParamProcessor(key, value)
   def as_string = new ToStringProcessor
}

class HTTP(val _url: String) {
   def apply[R](processor: Processor[R]): Validation[Exception, R] = {
      val url = new URL(_url)
      val connection = url.openConnection.asInstanceOf[HttpURLConnection]      
      processor(Bundle(connection, Map())) 
   }
}

object HTTP {
   def apply(url: String) = new HTTP(url)
}
