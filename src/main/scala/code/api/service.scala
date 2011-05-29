package code.api


import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.json.JsonAST._

import net.liftweb.util.Helpers._

object REST_Webservice extends RestHelper {

  serve ( "api"/ "item" prefix {

      // all the inventory
      case Nil JsonGet _ => JString("No action bound to this request")


      // Requests: /api/ID/[CMD]/[PARAMS+SIG], GET or POST

      // ID + Stats
      case AsLong(id) :: "stats" :: Nil JsonGet _ => JString("Action STATS for id="+id)
        //Customers.view(").stats : JValue

      // a particular item
      case AsLong(id) :: Nil JsonGet _ => JString("Select an action for id="+id) 

      case _ => JString("Bad request.")
    })

}
