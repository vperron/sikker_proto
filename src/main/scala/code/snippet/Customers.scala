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
//import record.field.{IntField, StringField}

// Just a helper class to help me get the fields I need
private object CustomerFieldHelper {
  import couchdb._
  import DocumentHelpers.jobjectToJObjectExtension
  import couchdb.DocumentHelpers.JObjectExtension

  implicit def toFieldHelper(obj: Customer): CustomerFieldHelper_Internal = new CustomerFieldHelper_Internal(obj)
  
  class CustomerFieldHelper_Internal(obj: Customer) {

    def field(name: String): String = obj.asJValue.getString(name) open_!

    def firstname : String = obj.field("first_name");
    def lastname : String = obj.field("last_name");
    def category : String = obj.field("info");
    def braceletid : String = obj.field("bracelet_id");


  }
}


class Customers extends Logger{
  import CustomerFieldHelper.toFieldHelper


   private object Data {
    import CustomerUtils._

    val (http, db) = init

    def customers = (Customer.all(_.limit(11)) open_!) toList
    
    val total_customers = ValueCell( (Customer.all(identity).open_! length) toString )

  }




  def totalCustomers(in: NodeSeq) = WiringUI.asText(in, Data.total_customers, JqWiringSupport.slideDown)


  def addCustomer(in: NodeSeq) = 
    <span class="span-4">{SHtml.link("/manage/edit", () => EditCustomer.selectedCustomer(Empty), Text("Add a customer"))}</span>


  def removeCustomer(cBox : Box[Customer]) : JsCmd  = {

    if (cBox isEmpty) return Noop

    val c = cBox open_!

    if(c canDelete_?) {
      c delete_! ;
      S.notice("Customer "+c.field("last_name")+" "+c.field("first_name")+" has been deleted.") 
    } 
    Noop
  }


  def htmlLine(c : Customer, classAttribute : String ) = {
          <tr class={classAttribute}>
          <td class="span-4" >{c.field("first_name")}</td>
          <td class="span-4" >{c.field("last_name")}</td>
          <td class="span-4"><i>{c.field("info")}</i></td>
          <td class="span-2">{SHtml.link("/manage/edit", () => EditCustomer.selectedCustomer(Full(c)), Text("Edit"))}</td>
          <td class="span-2 last">{SHtml.link("/manage/index", () => removeCustomer(Full(c)), Text("Remove")) }</td></tr>
  }

  def showLines : NodeSeq = {

    // Don't reload the database every time
    val customers = Data.customers

    S.notice(Data.total_customers.get + " customers were found in the CouchDB database.")

    val alternate =  for(i <- List.range(0,customers.length))  yield if(i % 2 == 0) "tr0" else "tr1"

    for((c, _class) <- customers zip alternate) 
      yield htmlLine(c, _class)

  }

}

object EditCustomer extends LiftScreen {
  import CustomerFieldHelper.toFieldHelper
  
  // RequestVar to retain selected user for the edition form
  object selectedCustomer extends SessionVar[Box[Customer]](Empty)

  override def screenTop = {
    if( selectedCustomer.isEmpty ) <b>New customer</b>
    else <b>Edition of customer : {(selectedCustomer open_!) firstname} {(selectedCustomer open_!) lastname }</b>
  }

  private val whence = S.referer openOr "/"

  val first_name = 
    new Field with StringField {
      def name = "First Name"

      override def default = selectedCustomer.map(_.firstname) openOr "";

      override def validations = valMinLen(1, "Too Short") _ ::
          valMaxLen(40, "Name Too Long") _ :: super.validations
    }

  val last_name = 
    new Field with StringField {
      def name = "Last Name"

      override def default = selectedCustomer.map(_.lastname) openOr "";
      
      override def validations = valMinLen(1, "Too Short")  _ ::
          valMaxLen(40, "Too Long") _ :: super.validations
    }

  val info = select("Category", selectedCustomer.map(_.category) openOr "0-4", List("0-4","4-24","Seniors"))

  def finish() {
    if(selectedCustomer isEmpty) { 
      val newRecord = Customer.createRecord.first_name(first_name).last_name(last_name).info(info);
      newRecord.bracelet_id(CouchUtils.generate_uuid);
      newRecord save;

      S.notice("Customer \""+first_name+" "+last_name+"\" has been added successfully.")
    } else {
      selectedCustomer.map(_.first_name(first_name))
      selectedCustomer.map(_.last_name(last_name))
      selectedCustomer.map(_.info(info))
      selectedCustomer.map(_.save)
      S.notice("Customer \""+first_name+" "+last_name+"\" has been edited successfully.")
    }

    S.redirectTo(whence)
  }
}
