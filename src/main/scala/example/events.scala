package info.whiter4bbit.events

import info.whiter4bbit.chttp.{client, HTTP, unsafe, HTTPResponse}
import info.whiter4bbit.chttp.oauth.{OAuthHelper, Token}

object Client extends App {
   import client._
   import OAuthHelper._
   import unsafe._

   val consumerKey = "psE5EHpeEU3m0eQVnWBS1A"
   val consumerSecret = "ZE9Tv1h7bvO1Kjt5uuy77SzJIzJzoA8VcsbWHQkv4"
   val h = new HTTP

   def resolveAuthToken(oauth: OAuthHelper) = {
      val token = h(oauth.getRequestToken).left.get.as_token
      printf("http://localhost:8080/oauth/authenticate?oauth_token=%s\n", token.left.get.token)
      val pin = Console.readLine
      h(oauth.getAccessToken(Some(pin), token)).left.get.as_token
   }

   def getAuthToken(oauth: OAuthHelper) = {
      resolveAuthToken(oauth)
   }

   def getMineEvents = {     
      val h = new HTTP        
      val oauth = new OAuthHelper(consumerKey, consumerSecret, apiURL="http://localhost:8080/oauth/") 
      val token = getAuthToken(oauth)
      printf("Token is: %s\n", token)
   }

   getMineEvents
}
