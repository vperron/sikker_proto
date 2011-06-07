package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import net.liftweb.couchdb.{CouchDB, Database, DatabaseInfo}
import dispatch.{Http, StatusCode}

import code.api._

import code.model._


/**
* A class that's instantiated early and run.  It allows the application
* to modify lift's environment
*/
class Boot extends Logger {
  def boot {

    // MySql connection for stats
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor = 
        new StandardDBVendor(Props.get("db.driver") open_!, 
        Props.get("db.url") open_!,
        Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }

    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, Stats)

    // where to search snippet
    LiftRules.addToPackages("code")

    // Hook for the webservice
    LiftRules.dispatch.append(REST_Webservice) // stateful -- associated with a servlet container session
    LiftRules.statelessDispatchTable.append(REST_Webservice)

    // Build SiteMap
    val entries = List(
      Menu.i("Index") / "index",
      Menu.i("Manage") / "manage" / "index",
      Menu.i("About") / "about")

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMap(SiteMap(entries:_*))

    // Use jQuery 1.4
    LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
    Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
    Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))


    //sets the default database for the application
    val (http, db) = CouchUtils.setup("customers")
    CouchDB.defaultDatabase = db

    debug(http(db info))

    def testRec1: Customer = {
      val id = CouchUtils.generate_uuid
      Customer.createRecord.first_name("Alice").last_name("Bobby").info("foo").bracelet_id(id)
    }

    val newCustomer = testRec1

    debug(newCustomer toXHtml)
    newCustomer.save

  }
}
