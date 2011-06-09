package code.snippet

import net.liftweb._
import http._
import util._
import common._
import Helpers._
import http._
import dispatch.{Http, StatusCode}
import js.JsCmd
import js.JsCmds._
import js.jquery._

import code.model._

import scala.xml.{NodeSeq, Text}
import json._
import record.field.{IntField, StringField}

// Just a helper class to help me get the fields I need
private object JObjectFieldHelper {
  import couchdb._
  import DocumentHelpers.jobjectToJObjectExtension
  import couchdb.DocumentHelpers.JObjectExtension

  implicit def toFieldHelper(obj: JObject): JObjectFieldHelper_Internal = new JObjectFieldHelper_Internal(obj)
  class JObjectFieldHelper_Internal(obj: JObject) {
    def field(name: String): String = obj.getString(name) open_!
  }
}


class Customers extends Logger{
  import JObjectFieldHelper.toFieldHelper


  private object Data {
    import CustomerUtils._

    val (http, db) = init

    def customers = {

      val Full(record) = http((db design(db_designName) view("informations") limit(5) descending) query) 

      record.rows.flatMap(_.value.asA[JObject]).toList
    }

    
    val total_customers = ValueCell( (Customer.all(identity).open_! length) toString )
  }


  // ValueCell of all customers
  val visibleCustomers = ValueCell[List[JObject]](Data.customers)

  // RequestVar to retain selected user for the edition form
  private object selectedCustomer extends RequestVar[JObject](new JObject(List()))


  def totalCustomers(in: NodeSeq) = WiringUI.asText(in, Data.total_customers, JqWiringSupport.slideDown)


  def addCustomer(in: NodeSeq) = <span>[Button to add a user]</span>

  def removeCustomer(c : JObject) : JsCmd  = {
    S.notice("Customer "+c.field("FirstName")+" removed.")
    Noop
  }

  def showLines = {

    // Don't reload the database every time
    val customers = visibleCustomers.get

    S.notice(Data.total_customers.get + " customers were found in the CouchDB database.")

    val alternate =  for(i <- List.range(0,customers.length))  yield if(i % 2 == 0) "tr0" else "tr1"

    "#customer_lines *" #> (
      for((c, _class) <- customers zip alternate) 
        yield  <tr class={_class + " span-16 last"}>
          <td class="span-4">{c.field("FirstName")}</td>
          <td class="span-4">{c.field("LastName")}</td>
          <td class="span-4"><i>{c.field("State")}</i></td>
          <td class="span-2">{SHtml.link("/manage/edit", () => selectedCustomer(c), Text("Edit"))}</td>
          <td class="span-2">{SHtml.a(Text("Delete"))(removeCustomer(c))}</td></tr>
    )
  }

}

