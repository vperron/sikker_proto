package code.snippet



import net.liftweb._
import http._
import util._
import http._
import Helpers._

import js.JsCmds._
import js.jquery._

import code.model._

import scala.xml.{NodeSeq, Text}
import json._
import JsonDSL._
import record.field.{IntField, StringField}

// guid is a way to attest uniqueness of this line's DIV in the current lift session. It's a RENDERING-oriented field.
// uuid is the CouchDB's uuid value of the newly created customer.
case class Line(guid: String, uuid: String, first_name: String, last_name: String, state: String, bracelets: Int)

class Customers {

  private object Data {

    val customers = Customer.all(_.key(JInt(30))).map(_.toList)
    
    val total_customers = ValueCell(customers.open_!.length)
    //val total_bracelets = TODO: list bracelets from database
  }

  //def total_bracelets(in: NodeSeq) = WiringUI.asText(in, Data.total_bracelets, JqWiringSupport.fade)

  def total_customers(in: NodeSeq) = WiringUI.asText(in, Data.total_customers, JqWiringSupport.slideDown)


  // Functions to render list of customers: highly inspired by simply liftweb example for wiring.
  def showLines = "* *" #> Data.customers.open_!.flatMap(u => <tr>{u.toXHtml}</tr>)




    private def getNewUuid = CouchUtils.generate_uuid


}

