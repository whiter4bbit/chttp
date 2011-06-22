package info.whiter4bbit
import info.whiter4bbit.chttp._
import info.whiter4bbit.chttp.oauth._

object CHttpExample {
   import Shortcuts._
   import OAuthShortcuts._

   val pinURL = "http://localhost:8080/authenticate"

   def main_(args: Array[String]) {
     val consumerKey = "iLj21H78hhg3JaAyBDlBFm3T2QlA5Ld0"
     val consumerSecret = "l06WIVOQBL6r4iQwRQJ7G8VEGY4XIFab"
     val oauth = new OAuthUtils(consumerKey, consumerSecret, "http://localhost:8080")     

     (for {     
         requestToken <- oauth.requestToken(post << oauth.getToken >> as_token);
	 pinResponse <- HTTP(pinURL)(post << param("token", requestToken.result.token) >> as_string);	 	 
	 accessToken <- oauth.accessToken(post << oauth.getToken(pinResponse.result, requestToken.result) >> as_token);	
	 info <- HTTP("http://localhost:8080/api/user/info")(get << oauth.getToken(accessToken.result) >> as_string)	 
     } yield {
         println(info.result)
     }) ||| ((e: Any) => println("Failure:" + e))
   }
}
