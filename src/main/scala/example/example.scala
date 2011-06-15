package info.whiter4bbit
import info.whiter4bbit.chttp._
import info.whiter4bbit.chttp.oauth._

object EventsAPI {
   import Shortcuts._
   import OAuthUtils._

   def main(args: Array[String]) {
     val consumerKey = "psE5EHpeEU3m0eQVnWBS1A"
     val consumerSecret = "ZE9Tv1h7bvO1Kjt5uuy77SzJIzJzoA8VcsbWHQkv4"
    
     val http = new HTTP("http://localhost:8080/oauth/request_token")
     val requestToken = http(post << request_token(consumerKey, consumerSecret) >> as_token)
  }
}
