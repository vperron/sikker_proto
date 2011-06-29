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

class Main {

  // Just to check whether db connection to couchdb works or not
  val db = CouchDB.defaultDatabase
  var Full(uuid_info) = CouchDBConfig.fetchFrom(new Database("_config"), "uuids")

  def about = "* *" #> "This code has been adapted for PJL380, 2011 - (C) Victor Perron"
  
}

