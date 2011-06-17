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
  val currentOffset = ValueCell(0)
  val range         = 5
  val page          = currentOffset.lift(_ / range)




  def makeTitle (in : NodeSeq) = {
    if ( selectedCustomer isEmpty ) goBack 

    val c = selectedCustomer open_!

    <span>{c.firstname + " " + c.lastname}</span>
  }

  def showLines: NodeSeq = {

    if ( selectedCustomer isEmpty ) goBack 

    val bracelet_id = (selectedCustomer open_!) braceletid

    val lines = Stats.findAll(By(Stats.bracelet_id, bracelet_id), 
                              OrderBy(Stats.timestamp, Descending), 
                              MaxRows(range),
                              StartAt(currentOffset.get))

    val htmlLines = for (line <- lines) yield htmlLine(line)


    switchViewGraph

    graphMode.open_! match {
      case true => {
        //"#graph_area *+" #> drawChart
        //Nil
        val data = drawChart
        error(data toString)
        data
        // Il y a donc encore ce putain de probleme des echelles qui se baladent...
      }
      case _ => { 
        titleLine ++ htmlLines
      }
    }
  }


  def backButton: NodeSeq  = { 
    <span class="span-4">{SHtml.link("/manage/index", () => selectedCustomer(Empty), Text("Back"))}</span>
  }

  def switchButton: NodeSeq  = { 
    <span class="span-4">{SHtml.link("/manage/stats",() => switchMode, Text(getModeString))}</span>
  }

  def shiftLeft(in: NodeSeq) = {
    SHtml.onEvents("onclick")(s => { toLeft; SetHtml("customer_stats", showLines)  })(in)
  }

  def shiftRight(in: NodeSeq) = {
    SHtml.onEvents("onclick")(s => { toRight; SetHtml("customer_stats", showLines)  })(in)
  }

  def follow(ns : NodeSeq) = {
    <i>{WiringUI.asText(page)(ns)} / {lineCount.open_! / range}</i>
  }



  private def drawChart = {
    val data_values: List[(Double,Double)] = for (i <- List.range (0, 140, 5))
        yield (i / 10.0, math.sin(i / 10.0) ) 

    val data_to_plot = new FlotSerie() {
        override val data = data_values
    } 


    Flot.init
    Flot.render ( "graph_area", List(data_to_plot), new FlotOptions {}, Flot.script(Nil))
  }





  /*
  * PRIVATE FUNCTIONS HERE
  */
  private def switchViewGraph {
    "#graph_area [style]" #> ("width:600px;height:400px;" + (if(!graphMode.open_!) "display:None"))
  }

  private def switchMode {
    val b = graphMode.open_! match  {
      case true => false
      case _ => true
    }
    graphMode(Full(b))
  }


  private def toLeft = {
    import scala.math._
    currentOffset.set(max(currentOffset.get - range, 0))
  }

  private def toRight = {
    import scala.math._
    val N = lineCount.open_!
    if(currentOffset.get + range < N) {
      currentOffset.set(currentOffset.get + range)
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


