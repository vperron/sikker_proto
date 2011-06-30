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


// SessionVar to retain selected user for the edition form
object selectedCustomer extends SessionVar[Box[Customer]](Empty)
object currentCustomers extends SessionVar[List[Customer]](CustomerUtils.reloadCustomers) 

object LoggedIn extends SessionVar(false)

class Customers extends Logger{

  import CustomerFieldHelper.toFieldHelper

  // Number of lines to be shown
  private val inc = 5

  private object filter extends SessionVar[String]("")
  private object range extends SessionVar[(Int, Int)]((0,inc))

  private def updateCount = SetHtml("pagecount", follow) & SetHtml("totalcount", <b>{customerCount}</b>)



  private def getCustomers : List[Customer] = {
    import scala.math._
    val customers = currentCustomers.get.filter ( _.hasString(filter.get.toLowerCase.trim) )
    for(i <- List.range(range.get._1, min(range.get._2, customers.length)))  yield customers.apply(i)
  }

  private def customerCount : Int = {
    import scala.math._
    currentCustomers.get.filter ( _.hasString(filter.get.toLowerCase.trim) ).length
  }


  /* 
  *  Snippet functions 
  */

  def shiftLeft(in: NodeSeq) = {
    SHtml.onEvents("onclick")(s => {
      import scala.math._
      range.atomicUpdate( v => { val tmp = max(v._1 - inc, 0); (tmp, min(tmp + inc, currentCustomers.get.length))})
      SetHtml("customer_lines", showLines) & updateCount 
    })(in)
  }

  def shiftRight(in: NodeSeq) = {
    SHtml.onEvents("onclick")(s => {
      import scala.math._
      range.atomicUpdate( v => {
        if(v._2 == currentCustomers.get.length) v;
        else {
           val tmp = min(v._2 + inc, currentCustomers.get.length); 
          (max(v._1 + inc, tmp - inc), tmp)}})
      SetHtml("customer_lines", showLines) & updateCount
    })(in)
  }

  def follow = <i>{range._1 / inc + 1} / {customerCount / inc + 1}</i>

  def totalCustomers = ("* *" #> <b>{customerCount}</b>)

  def addCustomer(in: NodeSeq) =  <span class="span-4">{SHtml.link("/manage/edit", () => selectedCustomer(Empty), Text("Add a customer"))}</span>

  def filterCustomers(s: String) : JsCmd = {
    import scala.math._
    filter.set(s)
    range atomicUpdate(v => (0, min(customerCount, 5)))
    SetHtml("customer_lines", showLines) & updateCount
  }

  // Creates the textBox that will filter key input events
  def lookForCustomer(in: NodeSeq) = {
    import http.js.JE
    val (name, js) = SHtml.ajaxCall(JE.JsRaw("this.value"), s => filterCustomers(s))
      <span>Search : {SHtml.ajaxText("", s => filterCustomers(s), "onkeyup" -> js.toJsCmd)}</span>
  }


  def removeCustomer(cBox : Box[Customer]) {
    if (cBox isEmpty) return

    val c = cBox open_!

    if(c canDelete_?) {
      import mapper.By
      
      Stats.bulkDelete_!!(By(Stats.bracelet_id,c braceletid))
      c delete_! ;
      currentCustomers.set(CustomerUtils.reloadCustomers)
      S.notice("Customer "+c.firstname+" "+c.lastname+" has been deleted.") 
    } 
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
    val alternate =  for(i <- List.range(0,getCustomers.length))  yield if(i % 2 == 0) "tr0" else "tr1"
    for((c, _class) <- getCustomers zip alternate) yield htmlLine(c, _class)
  }

  def logout = SHtml.link("/index", () => LoggedIn.set(false), Text("Sign out")) 

}

object LoginScreen extends LiftScreen with Logger {
  import CustomerFieldHelper.toFieldHelper
  

  override def screenTop = {
    <b>Please log in in order to access this zone.</b>
  }

  val login = new Field with StringField {
      def name = "Login"

      override def default = "";
      override def validations = valMinLen(1, "Too Short") _ ::
          valMaxLen(40, "Too Long") _ :: super.validations
  }

  val pwd = password("Password","",valMinLen(1,"Too Short"))

  def finish() {
    val admin = Admin.fetch(login);
    if(admin.isEmpty) return;
    if(admin.open_!.password.get.asInstanceOf[String] == pwd.get)  LoggedIn.set(true)

    S.redirectTo("/manage/")
    
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
      newRecord.private_key(randomString(Customer.private_key.maxLen));
      newRecord save;
      currentCustomers.set(CustomerUtils.reloadCustomers)
  

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
