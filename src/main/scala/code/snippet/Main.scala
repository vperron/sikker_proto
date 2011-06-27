package code.snippet

import scala.xml.{NodeSeq, Text}

import net.liftweb._
import util._
import common._
import http._
import couchdb._
import dispatch.{Http, StatusCode}

import Helpers._


import code.model._
import code.lib._

import json.Implicits.{int2jvalue, string2jvalue}
import json.JsonAST.{JObject}
import json.JsonDSL.{jobject2assoc, pair2Assoc, pair2jvalue}
import record.field.{IntField, StringField, DateTimeField}

import java.util.Date

class Main {
  lazy val date: Box[Date] = DependencyFactory.inject[Date] // inject the date

  // Just to check whether db connection to couchdb works or not
  val db = CouchDB.defaultDatabase
  var Full(uuid_info) = CouchDBConfig.fetchFrom(new Database("_config"), "uuids")

  def about = "* *" #> "This code has been adapted for PJL380, 2011 - (C) Victor Perron"
  


  /*
   lazy val date: Date = DependencyFactory.time.vend // create the date via factory

   def howdy = "#time *" #> date.toString
   */
}

