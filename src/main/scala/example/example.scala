package info.whiter4bbit

import info.whiter4bbit.chttp.{client, HTTP, unsafe, HTTPResponse}
import info.whiter4bbit.chttp.oauth.{OAuthHelper, Token}
import info.whiter4bbit.chttp.twitter.{TwitterOAuth, Twitter}

object TwitterApi {
   import client._
   import OAuthHelper._
   import Twitter._
   import unsafe._

   def askPin(token: Token):Either[String, Exception] = {
     printf("%s:\n", authorizationURL(token))
     try {
       Left(Console.readLine)
     } catch {
       case e: Exception => Right(e)
     }
   }

   def main(args: Array[String]) {
     val consumerKey = "psE5EHpeEU3m0eQVnWBS1A"
     val consumerSecret = "ZE9Tv1h7bvO1Kjt5uuy77SzJIzJzoA8VcsbWHQkv4"
     val h = new HTTP     
     val oauth = new OAuthHelper(consumerKey, consumerSecret, apiURL="http://localhost:8080/oauth/")
     val token = h(oauth.getRequestToken).left.get.as_token
     printf("http://localhost:8080/oauth/authenticate?oauth_token=%s\n", token.left.get.token)
     val pin = Console.readLine
     val t = h(oauth.getAccessToken(Some(pin), token)).left.get.as_token
     printf("Access token: %s\n", t)
     val res = h(client.GET("http://localhost:8080/events/mine") << oauth.accessToken(t)).to_string
     println("Resource response:" + res)
  }
}
