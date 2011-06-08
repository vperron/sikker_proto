package code.snippet

import net.liftweb._
import http._
import util._
import common._
import Helpers._
import http._
import dispatch.{Http, StatusCode}
import js.JsCmds._
import js.jquery._

import code.model._

import scala.xml.{NodeSeq, Text}
import json._
import record.field.{IntField, StringField}

class Customers extends Logger{
  import couchdb._
  import DocumentHelpers.jobjectToJObjectExtension


  // Just a helper class to help me get the fields I need
  private object JObjectFieldHelper {
    import couchdb.DocumentHelpers.JObjectExtension

    implicit def toFieldHelper(obj: JObject): JObjectFieldHelper_Internal = new JObjectFieldHelper_Internal(obj)
    class JObjectFieldHelper_Internal(obj: JObject) {
      def field(name: String): String = obj.getString(name) open_!
    }
  }




  private object Data {
    import CustomerUtils._

    def customers = {
      val (http, db) = init

      val Full(record) = http(db design(db_designName) view("informations") query) 

      record.rows.flatMap(_.value.asA[JObject]).toList
    }
    
    val total_customers = ValueCell(customers.length toString)
  }




  def totalCustomers(in: NodeSeq) = WiringUI.asText(in, Data.total_customers, JqWiringSupport.slideDown)


  def addCustomer(in: NodeSeq) = <span>[Button to add a user]</span>


  def showLines = {
    import JObjectFieldHelper.toFieldHelper

    S.notice(Data.total_customers.get + " customers were found in the CouchDB database.")

    // Data.customers.foreach{ r: JObject => S.notice(Printer.pretty(render(r))) }

    val alternate =  for(i <- List.range(0,Data.customers.length))  yield if(i % 2 == 0) "tr0" else "tr1"

    "* *" #> (
      for((c, _class) <- Data.customers zip alternate) 
        yield  <tr class={_class + " span-12 last"}>
          <td class="span-4">{c.field("FirstName")}</td>
          <td class="span-4">{c.field("LastName")}</td>
          <td class="span-4"><i>{c.field("State")}</i></td></tr>
    )
  }

}

