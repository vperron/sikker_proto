package code
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.json._
import net.liftweb.common._


class Stats extends KeyedMapper[Long, Stats] with Logger {
  def getSingleton = Stats 

  def primaryKeyField = id 

  // Implicit to generate well-typed errors from a  simple String.
  trait GenerateFieldError {
     self: FieldIdentifier =>
     implicit def strToListFieldError(msg: String): List[FieldError] =
       List(FieldError(self, msg))
   }


  def checkBraceletExists(s : String) : Boolean = {
    val (http, db) = CustomerUtils.init
    val bracelet = ((http(db design(CustomerUtils.db_designName) view("bracelets") key(JString(s)) query)).open_! rows) toList;
    if(bracelet.size != 0) true else false
  }




  object id extends MappedLongIndex(this) {
    override def dbNotNull_? = true
    override def dbColumnName = "id"
    override def required_? = true
    override def dbDisplay_? = false
  }

  object bracelet_id extends MappedString(this, 32) with GenerateFieldError {
    override def dbIndexed_? = true
    override def dbColumnName = "bracelet_id"
    override def required_? = true
    override def dbNotNull_? = true
    override def dbDisplay_? = false
    override def validate = if (checkBraceletExists(get)) Nil ; else dbColumnName+" : Failed parsing"
  }

  object timestamp extends MappedDateTime(this) {
    override def dbDisplay_? = false
    override def dbIndexed_? = true
    override def dbColumnName = "time"
    override def defaultValue = new _root_.java.util.Date()
    override def dbNotNull_? = true
    override def show_? = false
    override def displayName = "Time"
  }

  object temperature extends MappedDouble(this) with GenerateFieldError{
    override def dbIndexed_? = false
    override def dbColumnName = "temperature"
    override def required_? = true
    override def dbNotNull_? = true
    override def defaultValue = -1.0 // Allegedly false
    override def validate = if (get > 60 || get < 20) dbColumnName+" : Failed parsing" else Nil
    override def displayName = "Skin Temperature"
  }
  object cardio extends MappedDouble(this) with GenerateFieldError{
    override def dbIndexed_? = false
    override def dbColumnName = "cardio"
    override def required_? = true
    override def dbNotNull_? = true
    override def defaultValue = -1.0 // Allegedly false
    override def validate = if (get > 150 || get < 20)  dbColumnName+" : Failed parsing" else Nil
    override def displayName = "Cardiac Pulse"
  }
  object accel extends MappedDouble(this) with GenerateFieldError{
    override def dbIndexed_? = false
    override def dbColumnName = "accel"
    override def required_? = true
    override def dbNotNull_? = true
    override def defaultValue = -1.0 // Allegedly false
    override def validate = if (get > 10 || get < 0)  dbColumnName+" : Failed parsing" else Nil
    override def displayName = "Movement index"
  }
  object noise extends MappedDouble(this) with GenerateFieldError{
    override def dbIndexed_? = false
    override def dbColumnName = "noise"
    override def required_? = true
    override def dbNotNull_? = true
    override def defaultValue = -1.0 // Allegedly false
    override def validate = if (get > 100 || get < 0)  dbColumnName+" : Failed parsing" else Nil
    override def displayName = "Volume of noise"
  }


  def encodeAsJson(): JsonAST.JObject = Stats.encodeAsJson(this)
}


object Stats extends Stats with KeyedMetaMapper[Long, Stats] {
  def encodeAsJson(in: Stats): JsonAST.JObject = encodeAsJSON_!(in)
  def buildFromJson(json: JsonAST.JObject): Stats = decodeFromJSON_!(json, false)
}
