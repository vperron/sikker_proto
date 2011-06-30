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

  
  object graphMode extends SessionVar[Boolean](true)
  object lineCount   extends RequestVar[Long](countCustomers)
  object currentOffset extends SessionVar[Int](0)
  object currentGraph extends SessionVar[String]("temperature")
  val range         = 5
  val graph_range   = 50
  object currentRange extends SessionVar[Int](1)


  private def updateCount = SetHtml("pagecount", follow) 


  def makeTitle (in : NodeSeq) = {
    if ( selectedCustomer isEmpty ) goBack 

    val c = selectedCustomer open_!

    <span>{c.firstname + " " + c.lastname}</span>
  }

  def chooseBox (in: NodeSeq) : NodeSeq = {
    import http.js.JE

    if(graphMode get) {
      val (name, js) = SHtml.ajaxCall(JE.JsRaw("this.value"), s => {
          currentGraph.apply(s)
          SetHtml("graph_area", drawChart(s))
        })


      val selectValues = for(f <- Stats.formFields(Stats)) yield (f.dbColumnName, f.displayName)

      (SHtml.select(selectValues, Full(currentGraph get), s => drawChart(s), "onchange" -> js.toJsCmd))
    } else Nil
 
  }

  def showLines: NodeSeq = {

    if ( selectedCustomer isEmpty ) goBack 

    "#graph_area [style]" #> ("width:600px;height:400px;" + (if (!graphMode.get) "display:None;"))

    if ( graphMode get ) drawChart(currentGraph get)
    else getLines

  }


  def backButton: NodeSeq  = { 
    <span class="span-4">{SHtml.link("/manage/index", () => selectedCustomer(Empty), Text("Back"))}</span>
  }

  def switchButton: NodeSeq  = { 
    <span class="span-4">{SHtml.link("/manage/stats",() => switchMode, Text(getModeString))}</span>
  }

  def shiftLeft(in: NodeSeq) : NodeSeq = {
    if(!graphMode.get) SHtml.onEvents("onclick")(s => { toLeft ; SetHtml("customer_stats", showLines) & updateCount })(in)
    else SHtml.onEvents("onclick")(s => { 
          currentRange atomicUpdate(v => if(v > 1)  v - 1 else v)
          SetHtml("graph_area", drawChart(currentGraph get)) 
         })(in);
  }

  def shiftRight(in: NodeSeq) : NodeSeq = {
    if(!graphMode.get) SHtml.onEvents("onclick")(s => { toRight ; SetHtml("customer_stats", showLines) & updateCount })(in)
    else SHtml.onEvents("onclick")(s => { 
          currentRange atomicUpdate(v => if(v < (lineCount.get / graph_range))  v + 1 else v);
          SetHtml("graph_area", drawChart(currentGraph get)) 
         })(in);
  }

  def follow : NodeSeq = {
    if(!graphMode.get)
      <i>{currentOffset.get / range + 1} / {lineCount.get / range + 1}</i>
    else Nil
  }



  private def drawChart(s : String) : NodeSeq = {

    val bracelet_id = (selectedCustomer open_!) braceletid
    val values = Stats.findAll(By(Stats.bracelet_id, bracelet_id), 
                              OrderBy(Stats.timestamp, Descending), 
                              MaxRows(currentRange.get * graph_range))

    val data_to_plot = new FlotSerie() {
      override val data = for (stat <- values) yield (stat.timestamp.get.getTime : Double , stat.fieldByName(s).open_!.get : Double)

      override val label = Full(Stats.fieldByName(s).open_!.displayName)
     }

    val options : FlotOptions = new FlotOptions () {
      //val crosshair = Full(Map("mode" -> "xy"))

      override val xaxis = Full( new FlotAxisOptions() {
           override val mode = Full("time")
      })

   }
             

    Flot.init
    Flot.render ( "graph_area", List(data_to_plot), options, Flot.script(Nil))

  }





  /*
  * PRIVATE FUNCTIONS HERE
  */

  private def switchMode {
    graphMode atomicUpdate(if (_) false else true)
  }


  private def toLeft = {
    import scala.math._
    currentOffset atomicUpdate(v => max(v - range, 0))
  }

  private def toRight = {
    currentOffset atomicUpdate(v => if (v + range < lineCount.get) v + range else v)
  }

  

  private def getModeString = if(!graphMode.get) "Graph Mode" else "Raw Mode"

  private def countCustomers = Stats.count(By(Stats.bracelet_id, (selectedCustomer open_!) braceletid))

  private def goBack { debug("Going back..."); S.redirectTo(S.referer openOr "/manage/") }


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
                              StartAt(currentOffset get))

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


