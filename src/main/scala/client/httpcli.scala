package info.whiter4bbit.chttp

import java.net.{URL, HttpURLConnection}
import java.io.InputStream
import util._

trait Headers {
   def headers(request: HTTPRequest): HTTPRequest
}

object Headers {
   def apply(h: Map[String, String]): Headers = new Headers {
      def headers(req: HTTPRequest) = new HTTPRequest {
         override val url = req.url
         override val method = req.method
         override val headers = h
      }      
   }
}

trait HTTPRequest { req: HTTPRequest => 
   val url: String
   val method: String = "GET"
   val headers: Map[String, String] = Map()
   def <<(headers: Headers): HTTPRequest = headers.headers(req)
}  


object client {
   case class GET(u: String) extends HTTPRequest {   
      override val url = u
      override val method = "GET"
   }
   case class POST(u: String) extends HTTPRequest {   
      override val url = u
      override val method = "POST"
   }
}

object unsafe {
   implicit def unsafeLeft[A](either: Either[A, Exception]): A = either match {
        case Left(a) => a
	case _ => throw either.right.get
   }
}

class HTTPResponse(val responseCode: Int, 
                   val responseMessage: String, 
		   val inputStream: (()=>InputStream)) {
  
   def ok[A](handler: ((HTTPResponse => A))): Either[A, Exception] = if (responseCode == 200) {
      try {
         Left(handler(this))
      } catch {
         case e: Exception => Right(e)
      }
   } else {
      Right(new RuntimeException("Status is %d, expected 200".format(responseCode)))
   }

   def to_string = ok((resp: HTTPResponse) => {
      var str = ""
      val is = inputStream()     
      try {
         str = scala.io.Source.fromInputStream(is).mkString
      } finally {
         is.close
      }
      str      
    })
}

class HTTP {   
   def apply(request: HTTPRequest): Either[HTTPResponse, Exception] = {
      try {
         val url = new URL(request.url)
         val connection = url.openConnection.asInstanceOf[HttpURLConnection] 
         connection.setRequestMethod(request.method)
         connection.setDoOutput(true)
         val headers = request.headers
         headers.keys.foreach((key: String) => {      
           connection.setRequestProperty(key, headers(key))
         })
         Left(new HTTPResponse(responseCode = connection.getResponseCode,
                               responseMessage = connection.getResponseMessage,
                               inputStream = () => connection.getInputStream)) 
      } catch {
          case e: Exception => Right(e) 
      }
   } 
}
