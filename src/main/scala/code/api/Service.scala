package code.api


import net.liftweb._
import util._
import common._
import http._
import js.JsExp
import rest._
import json.JsonAST._
import code.model._
import util.Helpers._



object REST_Webservice extends RestHelper with Logger {
  
  val sizeOfChallenge = 200
  val tokenSize = 64

  private object authToken extends SessionVar[Box[String]](Empty)
  private object currentChallenge extends SessionVar[Box[String]](Empty)
  private object currentBracelet extends SessionVar[Box[String]](Empty)

  /**
  * An extractor for Tokens 
  */
  object Token {
    def unapply(in: String): Option[Boolean] = if (authToken === in) Full(true) else Empty 
  }
 
  private def genError(s: String) = JField("error", JString(s))


  private def allCustomers : LiftResponse = {
    val customers =  (Customer.all(identity)).open_!.toList.map(_.asJValue);
    customers.foldLeft[JValue](JNothing) { (a , b) => a ++ b  }
  }

  private def getCustomer(id : String) : LiftResponse = {
    val box = Customer.fetch(id)
    if (box isEmpty) genError("No record")
    else box.open_!.asJValue
  }

  private def getStats(bracelet_id: String) : LiftResponse = {
    import mapper._
    val s = Stats.findAll(By(Stats.bracelet_id,bracelet_id)).map(_.encodeAsJson);
    if (s isEmpty) genError("No record")
    else s.foldLeft[JValue](JNothing) { (a, b) => a ++ b }
  }

  private def addStatsLine(json: JValue) : LiftResponse = {
    import java.sql.SQLException
    
    val values = new JObject(json.children.map(_.asInstanceOf[JField]))
    val token = (values \ "auth_token")

    if(token == JNothing) return genError("No auth token specified.")
    
    if(!(authToken === token.asInstanceOf[JString].s)) return genError("Invalid auth token")

    val statObject = new JObject(values.obj.filter(_.name != "auth_token"))

    Stats.buildFromJson(statObject) asValid match {
      case Full(stat) => { 
        try { stat save } 
        catch {
          case e : SQLException => {error(e) ; return genError("Database exception : Malformed request.") }
          case e => return genError("Unknown exception occured.")
        }
        // Return correct time from server
        return JField("current_time",JInt((new java.util.Date).getTime))
      }
      case Failure(msg, _, _) => genError(msg)
      case _ => genError("Unexpected parsing error.")
    }

  }

  // Challenge = sha1(random + braceletId)
  private def generateChallenge(bracelet_id : String) : LiftResponse = {
    if(!Stats.checkBraceletExists(bracelet_id)) return genError("Unknown device");
    val ch = hashHex(randomString(sizeOfChallenge) + bracelet_id)
    currentBracelet.apply(Full(bracelet_id))
    currentChallenge.apply(Full(ch))
    JField("challenge",JString(ch))
  }

  private def getAssociatedKey(bracelet_id : String) : String = {
    import couchdb.DocumentHelpers.jobjectToJObjectExtension
    val (http, db) = CustomerUtils.init
    val query = ((http(db design(CustomerUtils.db_designName) view("bracelet_keys") key(JString(bracelet_id)) query)).open_! rows) toList;
    if (query.size != 1) throw new IllegalArgumentException("CouchDB corruption : Invalid query results.");
    val key = (query.apply(0).value.open_!).asInstanceOf[JObject].getString("Key")
    if (key isEmpty) throw new IllegalArgumentException("CouchDB corruption : No associated key was found.");
    return key.open_!
  }

  // encoded should be equal to sha1(challenge + key)
  private def checkGenerateToken(encodedChallenge : JValue) : LiftResponse = {
    import couchdb.DocumentHelpers.jobjectToJObjectExtension

    val s = (new JObject(encodedChallenge.children.map(_.asInstanceOf[JField]))).getString("challenge")

    if(currentChallenge.isEmpty || currentBracelet.isEmpty) return genError("Authentication failed")

    if (s === hashHex(currentChallenge.open_! + getAssociatedKey(currentBracelet open_!))) {
      currentChallenge.apply(Empty)
      val token = randomString(tokenSize)
      authToken.apply(Full(token))
      JField("auth_token",JString(token))
    } else genError("Authentication failed : invalid key.")
  }

  // REST service

  serve {

      /*
       * Authentication
       */

      // Generate a random challenge
      case "api" :: "auth" :: bracelet_id :: Nil JsonGet _ => generateChallenge(bracelet_id)
      // Receive the answer and if it's OK, return a session-wide token
      case "api" :: "auth" :: Nil JsonPut encodedChallenge -> _ => checkGenerateToken(encodedChallenge)


      // all the customers [if Token is recognized as valid]
      case "api" :: Token(token) :: Nil JsonGet _ => allCustomers

      // a particular customer
      case "api" :: Token(token) :: id :: Nil JsonGet _ => getCustomer(id)

      // customer stats
      case "api" :: "stats" :: Token(token) :: bracelet_id :: Nil JsonGet _ => getStats(bracelet_id)

      // For distant update
      // curl -X PUT -d '{ "foo": "bar" }' http://localhost:8080/api/stats/ID -H "Content-Type:application/json"
      case "api" :: "stats" :: Nil JsonPut json -> _ => addStatsLine(json)
  }

}
