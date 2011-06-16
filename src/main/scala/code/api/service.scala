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



  def allCustomers : LiftResponse = {
    val customers =  (Customer.all(identity)).open_!.toList.map(_.asJValue);
    customers.foldLeft[JValue](JNothing) { (a , b) => a ++ b  }
  }

  def getCustomer(id : String) : LiftResponse = {
    val box = Customer.fetch(id)
    if (box isEmpty) JString("No record")
    else box.open_!.asJValue
  }

  def getStats(bracelet_id: String) : LiftResponse = {
    import mapper._
    val s = Stats.findAll(By(Stats.bracelet_id,bracelet_id)).map(_.encodeAsJson);
    if (s isEmpty) JString("No record")
    else s.foldLeft[JValue](JNothing) { (a, b) => a ++ b }
  }

  def addStatsLine(json: JValue) : LiftResponse = {
    import java.sql.SQLException
  
    Stats.buildFromJson(new JObject(json.children.map(_.asInstanceOf[JField]))) asValid match {
      case Full(stat) => { 
        try { stat save } 
        catch {
          case e : SQLException => {error(e) ; return JString("Database exception : Malformed request.") }
          case e => return JString("Unknown exception occured.")
        }
        return stat.asJs;   
      }
      case Failure(msg, _, _) => JString(msg)
      case _ => JString("Unexpected parsing error.")
    }

  }

  serve {

      // all the customers
      case "api" :: Nil JsonGet _ => allCustomers

      // a particular customer
      case "api" :: id :: Nil JsonGet _ => getCustomer(id)

      // customer stats
      case "api" :: "stats" :: bracelet_id :: Nil JsonGet _ => getStats(bracelet_id)

      // For distant update
      // curl -X PUT -d '{ "foo": "bar" }' http://localhost:8080/api/stats/ID -H "Content-Type:application/json"
      case "api" :: "stats" :: Nil JsonPut json -> _ => addStatsLine(json)
  }

}
