package info.whiter4bbit.chttp.oauth

import info.whiter4bbit.chttp._
import info.whiter4bbit.chttp.util._
import scala.collection.mutable.{Map=>MMap}

import client._

case class Token(token: String, tokenSecret: String)

class OAuthHelper(val consumerKey: String, 
                  val consumerSecret: String, 
                  val apiURL: String = "https://api.twitter.com/oauth/", 
		  val requestTokenPostfix: String = "request_token",
		  val accessTokenPostfix: String = "access_token") {
   import OAuthHelper._

   def getRequestToken = 
      client.POST(apiURL + requestTokenPostfix) << requestToken

   def getAccessToken(pin: Option[String], token: Token) =
      client.POST(apiURL + accessTokenPostfix) << accessToken(pin, token)
   
   def requestToken = new Headers {
      def headers(req: HTTPRequest) = {       	
	val header = new OAuthHeader
	header.setConsumerKey(consumerKey)
	header.setURL(req.url)
	header.setMethod(req.method)
	header.setConsumerSecret(consumerSecret)

        Headers(header.build).headers(req)
      }
   }

   def accessToken(pin: Option[String], token: Token) = new Headers {
      def headers(req: HTTPRequest) = {
        val header = new OAuthHeader
	header.setConsumerKey(consumerKey)
	header.setURL(req.url)
	header.setMethod(req.method)
	header.setConsumerSecret(consumerSecret)
	header.setToken(Some(token.token))
	header.setPin(pin)
	header.setTokenSecret(Some(token.tokenSecret))

        Headers(header.build).headers(req)
      }
   }

   def accessToken(token: Token): Headers = accessToken(None, token) 
}

object OAuthHelper { 
   def responseParameters(resp: String): Map[String, String] = {   
      Map(resp.split('&').toList.map((pair: String) => {        
         val header = pair.split('=')
         (header(0), header(1))	    
      }):_*)
   }
   
   class TokenResponse(val response: HTTPResponse) {
      def as_token: Either[Token, Exception] = {
	 for {
	    body <- Either.LeftProjection(response.to_string)
	 } yield {
	    printf("[Body]%s\n", body)
	    val headers = responseParameters(body)
	    Token(headers("oauth_token"), headers("oauth_token_secret"))
	 }
      }
   }  

   implicit def responseToRequestToken(response: HTTPResponse): TokenResponse = 
       new TokenResponse(response)
}

