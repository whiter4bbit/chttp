package info.whiter4bbit.chttp.twitter

import info.whiter4bbit.chttp.oauth._
import info.whiter4bbit.chttp.util._
import info.whiter4bbit.chttp._

class TwitterOAuth(override val consumerKey: String, 
                   override val consumerSecret: String) 
      extends OAuthHelper(consumerKey,
                          consumerSecret,
                          apiURL = "https://api.twitter.com/oauth/",
                          requestTokenPostfix = "request_token",
                          accessTokenPostfix = "access_token") {
   def getTimeline(token: Token) = {
      client.GET("https://api.twitter.com/1/statuses/home_timeline.xml") << accessToken(token) 
   }
}
object Twitter {
   def authorizationURL(token: Token) = {
      "https://api.twitter.com/oauth/authorize?oauth_token=%s".format(token.token)
   }   
}
