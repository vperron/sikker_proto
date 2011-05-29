package code
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.couchdb._
import net.liftweb.record.field.{IntField, StringField, DateTimeField}

class Uuid private () extends CouchRecord[Uuid] {
  def meta = Uuid
  object algorithm extends StringField(this, 200)
}

object Uuid extends Uuid with CouchMetaRecord[Uuid]



class Stats extends KeyedMapper[String, Stats] {
  def getSingleton = Stats // what's the "meta" server

  def primaryKeyField = id



  val Full(uuid) = Uuid.fetchFrom(new Database("_config"), "uuids")

  val algorithm = uuid.algorithm.get 

  // Ne sert à rien mais montre que ça marche
  object id extends MappedStringIndex(this, algorithm match {
      case "sequential" => 32;
      case "random" => 32;
      case _ => 32;
    }
  )
  object timestamp extends MappedDateTime(this) {
    override def dbIndexed_? = false
  }
  object temperature extends MappedInt(this)
  object cardio extends MappedInt(this)
  object accel extends MappedInt(this)
  object noise extends MappedInt(this)
}


object Stats extends Stats with KeyedMetaMapper[String, Stats] {

}
