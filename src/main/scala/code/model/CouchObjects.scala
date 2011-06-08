package code.model

import net.liftweb._
import couchdb._
import http._
import common._

import mapper._
import json._
import JsonDSL._
import dispatch.{Http, StatusCode}
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



/*
Each bracelet has an unique ID that will track MySQL records for stats.
This ID will be generated with couchdb, of course, and stored inside of the bracelet at sell.
curl -X GET http://127.0.0.1:5984/_uuids
*/
object CustomerUtils extends Logger {

  val db_name = "customers"
  val db_designName = "first_design"

  private var db_design : JObject = 
  ("language" -> "javascript") ~
  ("views" -> (("informations" ->  ("map" -> """function(doc) { if (doc.type == 'Customer') { 
      emit(null, {FirstName: doc.first_name, LastName: doc.last_name}); } }""" )) ~
    ("foo"        -> (("map" -> "function(doc) { if (doc.type == 'Customer') { emit(doc.name, doc.age); } }") ~
    ("reduce" -> "function(keys, values) { return Math.max.apply(null, values); }")))))

  def init = {
    val (http, db) = CouchUtils.setup("customers");

    val design_rev = try { http(db.design(db_designName) fetch) \ "_rev" }
    catch { 
      case StatusCode(404, _) => debug("CouchDB: design "+db_designName+" does not exist for database "+db_name); new JString("");
    }

    debug(compact(render(design_rev)))

    db_design ~= ("_rev" -> compact(render(design_rev)).dropRight(1).drop(1))

    debug(Printer.pretty(render(db_design)))

    try { 
      http(db.design(db_designName) put db_design)
    } catch { 

      case StatusCode(409, _) => 
      debug("CouchDB: Update Design : Revision numbers did not match")

      case e => debug(e);
    }

  CouchDB.defaultDatabase = db

  (http, db)
}


def pretty(c : Customer) = compact(render(stripIdAndRev(c asJValue)))


  def testRec1: Customer = {
  val id = CouchUtils.generate_uuid
  Customer.createRecord.first_name("Alice").last_name("Bobby").info("foo").bracelet_id(id)
}
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




