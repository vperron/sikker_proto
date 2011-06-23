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


// RequestVar to retain selected user for the edition form
object selectedCustomer extends SessionVar[Box[Customer]](Empty)

class Customers extends Logger{

  import CustomerFieldHelper.toFieldHelper


   private object Data {

    private def max_customers = (Customer.all(identity)).open_! length

    private def checkLimits {
      val max = max_customers
      if(begin.get < 0) begin.set(0)
      if(end.get > max) end.set(max)

      page.set(begin.get/range + 1)
    }

    val range = 5
    private val begin = ValueCell(0)
    private val end = ValueCell(range)
    val filter = ValueCell("")
    val page = ValueCell(1)
    val totalpages = ValueCell(max_customers / range + 1)

    def customers = {

      checkLimits

      val s = filter.get.toLowerCase.trim

      if(s == "") { 
        val allcustomers = (Customer.all(identity) open_!) toList;
        CustomerUtils.debug("Begin = "+begin.get+" End = "+end.get)
        for(i <- List.range(begin get,end get))  yield allcustomers apply(i)
      }
      else ((Customer.all(identity) open_!) toList).filter ( _.hasString(s) )
    }
    

    val total_customers =  <b>{max_customers toString }</b>


    def shiftLeft = {
      import scala.math._
      begin.set(max(begin.get - range, 0))
      end.set(begin.get + range)
    }

    def shiftRight = {
      import scala.math._
      val max = max_customers
      if(begin.get + range < max) {
        begin.set(begin.get + range)
        end.set(min(begin.get + range, max))
      }
    }

  }


  def shiftLeft(in: NodeSeq) = {
    SHtml.onEvents("onclick")(s => {
      Data.shiftLeft
      SetHtml("customer_lines", showLines)
    })(in)
  }

  def shiftRight(in: NodeSeq) = {
    SHtml.onEvents("onclick")(s => {
      Data.shiftRight
      SetHtml("customer_lines", showLines)
    })(in)
  }

  def follow(ns : NodeSeq) = {
    <i>{WiringUI.asText(Data.page)(ns)} / {WiringUI.asText(Data.totalpages)(ns)}</i>

  }


  def totalCustomers(in: NodeSeq) = Data.total_customers


  def addCustomer(in: NodeSeq) = 
    <span class="span-4">{SHtml.link("/manage/edit", () => selectedCustomer(Empty), Text("Add a customer"))}</span>


  def filterCustomers(s: String) : JsCmd = {
    Data.filter.set(s)
    SetHtml("customer_lines", showLines)
  }

  def lookForCustomer(in: NodeSeq) = {
    import http.js.JE
    val (name, js) = SHtml.ajaxCall(JE.JsRaw("this.value"), s => filterCustomers(s))
      <span>Search : {SHtml.ajaxText("", s => filterCustomers(s), "onkeyup" -> js.toJsCmd)}</span>
  }


  def removeCustomer(cBox : Box[Customer]) : JsCmd  = {

    if (cBox isEmpty) return Noop

    val c = cBox open_!

    if(c canDelete_?) {
      import mapper.By
      
      Stats.bulkDelete_!!(By(Stats.bracelet_id,c braceletid))
      c delete_! ;
      S.notice("Customer "+c.firstname+" "+c.lastname+" has been deleted.") 
    } 
    Noop
  }


  def htmlLine(c : Customer, classAttribute : String ) = {
          <tr class={classAttribute}>
          <td class="span-4" >{c.field("first_name")}</td>
          <td class="span-4" >{c.field("last_name")}</td>
          <td class="span-4"><i>{c.field("info")}</i></td>
          <td class="span-2">{SHtml.link("/manage/stats", () => selectedCustomer(Full(c)), Text("Stats"))}</td>
          <td class="span-2">{SHtml.link("/manage/edit", () => selectedCustomer(Full(c)), Text("Edit"))}</td>
          <td class="span-2 last">{ SHtml.link("/manage/index", () => removeCustomer(Full(c)), Text("Remove")) }</td></tr>
  }

  def showLines : NodeSeq = {

    val customers = Data.customers

    val alternate =  for(i <- List.range(0,customers.length))  yield if(i % 2 == 0) "tr0" else "tr1"

    for((c, _class) <- customers zip alternate) yield htmlLine(c, _class)

  }

}

object EditCustomer extends LiftScreen {
  import CustomerFieldHelper.toFieldHelper
  

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
          valMaxLen(40, "Too Long") _ :: super.validations
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
