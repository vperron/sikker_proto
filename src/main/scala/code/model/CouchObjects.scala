package code.model

import net.liftweb.couchdb._
import net.liftweb.json.JsonAST._
import net.liftweb.record.field.{IntField, StringField, DateTimeField}



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

  object name extends StringField(this, 200)
  object state extends StringField(this, 200)
  object bracelets extends JSONBasicArrayField(this) {

    object sql_id extends JInt(0)
    object state extends JString("")

  }


}

object Customer extends Customer with CouchMetaRecord[Customer] {
  override def createRecord = new Customer
}




