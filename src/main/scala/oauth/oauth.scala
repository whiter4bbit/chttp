package info.whiter4bbit.chttp.oauth

import info.whiter4bbit.chttp._
import scalaz._
import Scalaz._

case class Token(val token: String, val tokenSecret: String)

class RequestTokenProcessor(val consumerKey: String, val consumerSecret: String) extends BundleProcessor {
    def apply(bundle: Bundle): Validation[Exception, Bundle] = {
        val header = new OAuthHeader
        header.setConsumerKey(consumerKey)
        header.setURL(bundle.conn.getURL().toString)	
        header.setMethod(bundle.conn.getRequestMethod())
        header.setConsumerSecret(consumerSecret)

        bundle.conn.setRequestProperty("Authorization", header.build) 	
        bundle.success
    }
}

class AccessTokenProcessor(val consumerKey: String, val consumerSecret: String, pin: Option[String], token: Token) 
       extends BundleProcessor {
     def this(consumerKey: String, consumerSecret: String, token: Token) = {
        this(consumerKey, consumerSecret, None, token)
     }
     def apply(bundle: Bundle): Validation[Exception, Bundle] = {
        val header = new OAuthHeader
        header.setConsumerKey(consumerKey)
        header.setURL(bundle.conn.getURL().toString)
        header.setMethod(bundle.conn.getRequestMethod)
        header.setConsumerSecret(consumerSecret)
        header.setToken(Some(token.token))
        header.setPin(pin)
        header.setTokenSecret(Some(token.tokenSecret))

        bundle.conn.setRequestProperty("Authorization", header.build)
        bundle.success
     }
}

object OAuthUtils {
   import Shortcuts._

   def responseParameters(resp: String): Map[String, String] = {   
      Map(resp.split('&').toList.map((pair: String) => {        
         val header = pair.split('=')
         (header(0), header(1))     
      }):_*)
   }
   def request_token(consumerKey: String, consumerSecret: String) = 
          new RequestTokenProcessor(consumerKey, consumerSecret)
   def access_token(consumerKey: String, consumerSecret: String, pin: Option[String], token: Token) = 
          new AccessTokenProcessor(consumerKey, consumerSecret, pin, token)
   def as_token = as_string.!!((s: String) => {
       val param = responseParameters(s) 
       Token(param("oauth_token"), param("oauth_token_secret"))
   })
}
