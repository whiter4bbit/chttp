package info.whiter4bbit.chttp.events

import info.whiter4bbit.chttp._
import info.whiter4bbit.chttp.oauth._
import Shortcuts._
import OAuthShortcuts._

import scalaz._
import Scalaz._
import net.liftweb.json.scalaz.JsonScalaz._
import net.liftweb.json.Serialization.write
import net.liftweb.json._
import net.liftweb.json.JsonDSL._

class User(val login: String, val password: String, val consumerKey: String, val consumerSecret: String)

object User {
   def apply(login: String, password: String, consumerKey: String, consumerSecret: String) = {
      new User(login, password, consumerKey, consumerSecret)
   }
   def apply(json: JValue) = {
      for {
         login <- field[String]("login")(json);
	 password <- field[String]("password")(json);
	 consumerKey <- field[String]("consumerKey")(json);
	 consumerSecret <- field[String]("consumerSecret")(json)
      } yield {
         new User(login, password, consumerKey, consumerSecret)
      }
   }
}

import java.util.Date

class Event(val id: Option[String], val name: String,val description: String,val startDate: Date,val endDate: Date)

object Event {
   implicit val formats = DefaultFormats

   implicit def dateJSON(implicit f: Formats): JSON[Date] = new JSON[Date] {
      def read(json: JValue) = json match {
         case JString(x) => formats.dateFormat.parse(x).map((date) => {
	    date.success
	 }).getOrElse(UnexpectedJSONError(x, classOf[JString]).fail.liftFailNel)
	 case x => UnexpectedJSONError(x, classOf[JString]).fail.liftFailNel
      }
      def write(date: Date) = {
         JString(f.dateFormat.format(date))
      }
   }

   def apply(name: String, description: String, startDate: Date, endDate: Date) = {
      new Event(None, name, description, startDate, endDate)
   }
   def apply(json: JValue) = {
      for {
         id <- field[Option[String]]("id")(json);
         name <- field[String]("name")(json);
	 description <- field[String]("description")(json);
	 startDate <- field[Date]("startDate")(json);
	 endDate <- field[Date]("endDate")(json)
      } yield {
         new Event(id, name, description, startDate, endDate)	 
      }
   }
}

trait EventsConfig { 
   def host: String
}

trait OAuthEventsConfig extends EventsConfig {
   val oauth: OAuthUtils
}

trait PublicAPI { self: EventsConfig =>
   val addUserURL = HTTP(self.host + "/api/users/add")

   def addUser(login: String, password: String) = {
      val json = compact(render(Map("login" -> login, "password" -> password)))
      for { 
         result <- addUserURL(post << param("user", json) >> as_string);
         user <- User(parse(result.result))
      } yield {
         user
      }
   }   
}

trait ProtectedAPI { self: OAuthEventsConfig =>
   val userInfoURL = HTTP(self.host + "/api/user/info")
   val addEventURL = HTTP(self.host + "/api/events/add")
   val mineEventsURL = HTTP(self.host + "/api/events/mine")
   val attendEventURL = HTTP(self.host + "/api/events/attend")

   implicit val formats = DefaultFormats

   def userInfo(token: Token) = {
      for { 
         result <- userInfoURL(get << oauth.getToken(token) >> as_string)
         user <- User(parse(result.result))
      } yield {
         user
      }
   }   

   def addEvent(event: Event)(token: Token) = {
      for {
         result <- addEventURL(post << oauth.getToken(token) << param("event", write(event)) >> as_string)
	 inserted <- Event(parse(result.result))
      } yield {
         inserted
      }
   }

   def mineEvents(token: Token) = {
      for {
         result <- mineEventsURL(get << oauth.getToken(token) >> as_string)
      } yield {
         result.result
      }
   }

   def attendEvent(eventId: String)(token: Token) = {
      val json = compact(render(Map("eventId" -> eventId)))
      for {
         result <- attendEventURL(post << oauth.getToken(token) << param("event", json) >> as_string)
      } yield {
         result.result
      }
   }
}

trait AuthTokenResolver { self: OAuthEventsConfig => 
   val pinURL = HTTP(self.host + "/authenticate")

   def getAuthToken = {
      for {     
         requestToken <- oauth.requestToken(post << oauth.getToken >> as_token);
	 pinResponse <- pinURL(post << param("token", requestToken.result.token) >> as_string); 
	 accessToken <- oauth.accessToken(post << oauth.getToken(pinResponse.result, requestToken.result) >> as_token)	
      } yield {
         accessToken.result
      }
   }
}

class EventsAPI(val consumerKey: String, 
                val consumerSecret: String, 
		val host_ : String) extends ProtectedAPI with OAuthEventsConfig {
   override def host = host_
   override val oauth = new OAuthUtils(consumerKey, consumerSecret, host)
}

object EventsAPI {
   def publicAPI(host_ : String): PublicAPI = {   
      object newApi extends PublicAPI with EventsConfig {
         override def host = host_
      }
      newApi 
   }

   def protectedAPI(consumerKey: String, consumerSecret: String, host: String) = {
      new EventsAPI(consumerKey, consumerSecret, host) with AuthTokenResolver
   }
}

object TestEventsAPI extends App {
   val api = EventsAPI.protectedAPI("iLj21H78hhg3JaAyBDlBFm3T2QlA5Ld0", 
                                    "l06WIVOQBL6r4iQwRQJ7G8VEGY4XIFab", 
			            "http://localhost:8080")
   val token = api.getAuthToken.toOption.get
   println(api.userInfo(token))
   println(api.mineEvents(token))
   println(api.attendEvent("4e009428a5db2aaa7e28c250")(token))
   println(api.attendEvent("4e009428a5db2aaa7e28c251")(token))  
}
