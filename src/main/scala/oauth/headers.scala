package info.whiter4bbit.chttp.oauth

import scala.collection.mutable.{Map=>MMap}
import info.whiter4bbit.chttp.oauth.util._
import info.whiter4bbit.chttp.oauth.util.HeaderUtils._

class OAuthHeader {
   var consumerKey: String = "" 
   var token: Option[String] = None
   var pin: Option[String] = None
   var tokenSecret: Option[String] = None
   var method: String = ""
   var url: String = ""
   var consumerSecret: String = ""
   def setConsumerKey(consumerKey: String) = {
      this.consumerKey = consumerKey	 
      this
   }
   def setToken(token: Option[String]) = {
      this.token = token
      this
   }      
   def setPin(pin: Option[String]) = {
      this.pin = pin
      this
   }
   def setMethod(method: String) = {
      this.method = method
      this
   }
   def setURL(url: String) = {
      this.url = url
      this
   }
   def setTokenSecret(tokenSecret: Option[String]) = {
      this.tokenSecret = tokenSecret
      this
   }
   def setConsumerSecret(consumerSecret: String) = {
      this.consumerSecret = consumerSecret
      this
   }
   def build = {
      val timestamp = System.currentTimeMillis() / 1000
      val oauthNonce = System.nanoTime.toString         
      val params = MMap("oauth_consumer_key" -> consumerKey,
                        "oauth_callback" -> "oob",	 
     	                "oauth_nonce" -> oauthNonce,			
                        "oauth_signature_method" -> "HMAC-SHA1",
                        "oauth_timestamp" -> timestamp.toString)
      pin.map((v: String) => { params += (("oauth_verifier", v)) })
      token.map((v: String) => { params += (("oauth_token", v)) })
      var oauthData = OAuthUtil.encode(params.toMap, url, method)	
      val secretKey = consumerSecret + "&" + tokenSecret.getOrElse("")	
      val oauthSignature = OAuthUtil.hmac(oauthData, secretKey)
      params += (("oauth_signature", oauthSignature))
      val normalizedParams = params.map(e => (e._1, wrap(e._2))) 
      "OAuth " + OAuthUtil.encodeParameters(normalizedParams.toMap, ",")
   }
}


