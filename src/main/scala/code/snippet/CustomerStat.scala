package code.snippet

import code.model._

import net.liftweb._
import scala.xml.{NodeSeq, Text}
import mapper._
import common.{Box, Empty, Full, Logger}
import http.{S, SHtml, WiringUI, RequestVar, SessionVar}
import http.js.JsCmds._
import util.ValueCell
import util.Helpers._
import widgets.flot._




class CustomerStats extends Logger {

  import CustomerFieldHelper.toFieldHelper

  
  object graphMode extends SessionVar[Box[Boolean]](Full(true))
  object lineCount   extends RequestVar[Box[Long]](Full(countCustomers))
  object currentOffset extends SessionVar[Box[Int]](Full(0))
  val currentGraph = ValueCell("temperature")
  val range         = 5
  val graph_range   = 50
  object currentRange extends SessionVar[Int](1)




  def makeTitle (in : NodeSeq) = {
    if ( selectedCustomer isEmpty ) goBack 

    val c = selectedCustomer open_!

    <span>{c.firstname + " " + c.lastname}</span>
  }

  def chooseBox (in: NodeSeq) : NodeSeq = {
    import http.js.JE

    if(graphMode.open_!) {
      val (name, js) = SHtml.ajaxCall(JE.JsRaw("this.value"), s => SetHtml("graph_area", drawChart(s)))

      val selectValues = List( ("temperature", "Temperature"),
                                ("cardio", "Cardiology"),
                                ("accel", "Movements"),
                                ("noise", "Noise"))

      (SHtml.select(selectValues, Full(currentGraph get), s => drawChart(s), "onchange" -> js.toJsCmd))
    } else Nil
 
  }

  def showLines: NodeSeq = {

    if ( selectedCustomer isEmpty ) goBack 

    val isGraphMode_? = graphMode.open_!

    "#graph_area [style]" #> ("width:600px;height:400px;" + (if (!isGraphMode_?) "display:None;"))

    if ( isGraphMode_? ) drawChart(currentGraph get)
    else getLines

  }


  def backButton: NodeSeq  = { 
    <span class="span-4">{SHtml.link("/manage/index", () => selectedCustomer(Empty), Text("Back"))}</span>
  }

  def switchButton: NodeSeq  = { 
    <span class="span-4">{SHtml.link("/manage/stats",() => switchMode, Text(getModeString))}</span>
  }

  def shiftLeft(in: NodeSeq) : NodeSeq = {
    if(!graphMode.open_!) SHtml.onEvents("onclick")(s => { toLeft ; SetHtml("customer_stats", showLines) })(in)
    else <span class="span-4">{SHtml.link("/manage/stats",() => currentRange atomicUpdate(v => if(v > 1)  v - 1 else v), Text("-"))}</span>
  }

  def shiftRight(in: NodeSeq) : NodeSeq = {
    if(!graphMode.open_!) SHtml.onEvents("onclick")(s => { toRight ; SetHtml("customer_stats", showLines) })(in)
    else <span class="span-4">{SHtml.link("/manage/stats",() => currentRange atomicUpdate(v => if(v < lineCount.open_!)  v + 1 else v), Text("+"))}</span>
  }

  def follow(ns : NodeSeq) : NodeSeq = {
    if(!graphMode.open_!)
      <i>{currentOffset.open_! / range} / {lineCount.open_! / range}</i>
    else Nil
  }



  private def drawChart(s : String) : NodeSeq = {

    val bracelet_id = (selectedCustomer open_!) braceletid
    val values = Stats.findAll(By(Stats.bracelet_id, bracelet_id), 
                              OrderBy(Stats.timestamp, Descending), 
                              MaxRows(currentRange.get * graph_range))

    val data_to_plot = new FlotSerie() {
      override val data = for ((i, stat) <- List.range(0, values.length) zip values) yield (i : Double , stat.fieldByName(s).open_!.get : Double)
    }

    Flot.init
    Flot.render ( "graph_area", List(data_to_plot), new FlotOptions {}, Flot.script(Nil))

  }





  /*
  * PRIVATE FUNCTIONS HERE
  */

  private def switchMode {
    val b = graphMode.open_! match  {
      case true => false
      case _ => true
    }
    graphMode(Full(b))
  }


  private def toLeft = {
    import scala.math._
    currentOffset(Full(max(currentOffset.open_! - range, 0)))
  }

  private def toRight = {
    import scala.math._
    val N = lineCount.open_!
    if(currentOffset.open_! + range < N) {
      currentOffset(Full(currentOffset.open_! + range))
    }
  }

  

  private def getModeString = if(!graphMode.open_!) "Graph Mode" else "Raw Mode"

  private def countCustomers = Stats.count(By(Stats.bracelet_id, (selectedCustomer open_!) braceletid))

  private def goBack { debug("Going back..."); S.redirectTo(S.referer openOr "/") }


  private def titleLine = {
          <div class="span-20 last" >
          <span class="span-8">Time</span>
          <span class="span-2">Temp</span>
          <span class="span-2">BpM</span>
          <span class="span-2">Mov</span>
          <span class="span-2">dB</span>
          <hr />
          </div>
  }

  private def getLines : NodeSeq = {
    val bracelet_id = (selectedCustomer open_!) braceletid

    val lines = Stats.findAll(By(Stats.bracelet_id, bracelet_id), 
                              OrderBy(Stats.timestamp, Descending), 
                              MaxRows(range),
                              StartAt(currentOffset.open_!))

    titleLine ++ (for (line <- lines) yield htmlLine(line))
  }

  private def htmlLine(s : Stats) = {
    <span class="span-20 last">
      <span class="span-8">{s.timestamp.toString}</span>
      <span class="span-2">{s.temperature.get}</span>
      <span class="span-2">{s.cardio.get}</span>
      <span class="span-2">{s.accel.get}</span>
      <span class="span-2">{s.noise.get}</span>
    </span>
  }

}


