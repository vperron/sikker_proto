package code.model

import net.liftweb.couchdb._
import net.liftweb.http._
import net.liftweb.common._

import net.liftweb.json.JsonAST._
import net.liftweb.mapper._
import dispatch.{Http, StatusCode}
import net.liftweb.record.field.{IntField, StringField, DateTimeField}


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



object CouchUtils {


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
    ( try { 
      Http(database info) 
    } catch { 
      case StatusCode(404, _) => 
        S.warning("CouchDB: database "+dbName+" does not seem to exist.")
        Http(database create)
    }
    ) 
    (http, database)
  }




}



/*
        Each bracelet has an unique ID that will track MySQL records for stats.
        This ID will be generated with couchdb, of course, and stored inside of the bracelet at sell.
        curl -X GET http://127.0.0.1:5984/_uuids
*/


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
  object bracelet_id extends JSONBasicArrayField(this, 32) 

}

object Customer extends Customer with CouchMetaRecord[Customer] 




