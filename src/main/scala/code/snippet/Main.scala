package code.snippet

import scala.xml.{NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import java.util.Date
import code.lib._
import Helpers._

import net.liftweb.couchdb._
import dispatch.{Http, StatusCode}

import code.model._

import net.liftweb.json.Implicits.{int2jvalue, string2jvalue}
import net.liftweb.json.JsonAST.{JObject}
import net.liftweb.json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import net.liftweb.record.field.{IntField, StringField, DateTimeField}


class Main {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // Just to check whether db connection to couchdb works or not
  val db = CouchDB.defaultDatabase
  var Full(uuid) = Uuid.fetchFrom(new Database("_config"), "uuids")
  S.error(uuid.algorithm.get )

  // replace the contents of the element with id "time" with the date
  def howdy = "#time *" #> date.map(_.toString)


  /*
   lazy val date: Date = DependencyFactory.time.vend // create the date via factory

   def howdy = "#time *" #> date.toString
   */
}

