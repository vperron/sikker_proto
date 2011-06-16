package code.model

import net.liftweb._
import couchdb._
import http._
import common._

import mapper._
import json._
import JsonDSL._
import dispatch.{Http, Request, StatusCode}
import record.field.{IntField, StringField, DateTimeField}
import DocumentHelpers.stripIdAndRev


class CouchDBConfig private () extends CouchRecord[CouchDBConfig] {
  def meta = CouchDBConfig
  object algorithm extends StringField(this, 200)
}

object CouchDBConfig extends CouchDBConfig with CouchMetaRecord[CouchDBConfig]



class Uuids private () extends CouchRecord[Uuids] {
  def meta = Uuids
  object uuids extends JSONBasicArrayField(this)
}

object Uuids extends Uuids with CouchMetaRecord[Uuids]



object CouchUtils extends Logger {


  def algorithm = {
    val Full(uuid) = CouchDBConfig.fetchFrom(new Database("_config"), "uuids")
    uuid.algorithm.get
  }


  def generate_uuid : String = {
    val Full(uuid) = Uuids.fetchFrom(new Database("_uuids"), "uuids") 
    val id : String = uuid.uuids.asJValue.apply(0) match { 
      case JString(s: String) => s
      case _ => "No uuid found."
    }
    id
  }

  def setup(dbName : String) = {
    val http = new Http
    val database = new Database(dbName)
    try { 
      http(database info) 
    } catch { 
      case StatusCode(404, _) => 
      debug("CouchDB: database "+dbName+" does not seem to exist.")
      http(database create)
    }
    (http, database)
  }
}


// Just a helper class to help me get the fields I need
object CustomerFieldHelper {
  import couchdb._
  import DocumentHelpers.jobjectToJObjectExtension
  import couchdb.DocumentHelpers.JObjectExtension

  implicit def toFieldHelper(obj: Customer): CustomerFieldHelper_Internal = new CustomerFieldHelper_Internal(obj)
  
  class CustomerFieldHelper_Internal(obj: Customer) {

    def field(name: String): String = obj.asJValue.getString(name) open_!

    def firstname : String = obj.field("first_name");
    def lastname : String = obj.field("last_name");
    def category : String = obj.field("info");
    def braceletid : String = obj.field("bracelet_id");

    def hasString(s : String) : Boolean = {
          (obj.lastname.toLowerCase.indexOf(s) != -1) ||
          (obj.firstname.toLowerCase.indexOf(s) != -1) ||
          (obj.category.toLowerCase.indexOf(s) != -1)
    }
  }
}

/*
Each bracelet has an unique ID that will track MySQL records for stats.
This ID will be generated with couchdb, of course, and stored inside of the bracelet at sell.
curl -X GET http://127.0.0.1:5984/_uuids
*/
object CustomerUtils extends Logger {

  private val db_name = "customers"

  val db_designName = "first_design"

  private var db_design : JObject = 
  ("language" -> "javascript") ~
  ("views" -> (
    ("informations" ->  
      ("map" -> """function(doc) { if (doc.type == 'Customer') { 
      emit(doc.info, {FirstName: doc.first_name, LastName: doc.last_name, State: doc.info}); } }""" )) ~
    ("bracelets"  -> (
      ("map" -> """function(doc) { if (doc.type == 'Customer') { 
        emit(doc.bracelet_id, {CustomerId: doc._id, BraceletId: doc.bracelet_id}); } }""")))))


  def init(update_? : Boolean = false) : (Http, Database) = {
    val (http, db) = CouchUtils.setup(db_name)
    if(update_?) {
      val design_rev = getRevision(db design(db_designName)) 
      val updated_design = db_design ~ ("_rev" -> compact(render(design_rev)).dropRight(1).drop(1))
      // Update view using correct revision number
      debug(Printer.pretty(render(updated_design)))
      try { http(db.design(db_designName) put updated_design) } catch { 
        case StatusCode(409, _) => 
        debug("CouchDB: Update Design : Revision numbers did not match")
        case e => debug(e);
      }
    }
    CouchDB.defaultDatabase = db
    (http, db)
  }

  def init : (Http, Database) = init(false)

  def getRevision(req : FetchableAsJObject) = {
    val (http, db) = CouchUtils.setup(db_name);
    try { http(req fetch) \ "_rev" }
    catch { 
      case StatusCode(404, _) => debug("CouchDB: design "+db_designName+" does not exist for database "+db_name); new JString("");
    }
  }

  def pretty_print(c : Customer) = Printer.pretty(render(stripIdAndRev(c asJValue)))

}



/*
Each Customer has several properties, and is associated with
a list of the Bracelets he possesses.
The exact list of properties can be determined later.
*/



class Customer extends CouchRecord[Customer] {
  def meta = Customer

  object first_name extends StringField(this, 200)
  object last_name extends StringField(this, 200)
  object info extends StringField(this, 200)
  object bracelet_id extends StringField(this, 32) 

}

object Customer extends Customer with CouchMetaRecord[Customer] 




