package bootstrap.liftweb

import net.liftweb._
import util._
import Helpers._

import common._
import http._
import js.jquery.JQueryArtifacts
import sitemap._
import Loc._
import mapper._

import code.model._
import code.lib._
import net.liftmodules.JQueryModule

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?) {
      val vendor =
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
          Props.get("db.url") openOr
            "jdbc:h2:lift_proto.db;AUTO_SERVER=TRUE",
          Props.get("db.user"), Props.get("db.password"))

      LiftRules.unloadHooks.append(vendor.closeAllConnections_! _)

      DB.defineConnectionManager(DefaultConnectionIdentifier, vendor)
    }


    // Use Lift's Mapper ORM to populate the database
    // you don't need to use Mapper to use Lift... use
    // any ORM you want
    Schemifier.schemify(true, Schemifier.infoF _, User)
    Schemifier.schemify(true, Schemifier.infoF _, Language)
    Schemifier.schemify(true, Schemifier.infoF _, Localization)
    Schemifier.schemify(true, Schemifier.infoF _, Article)
    Schemifier.schemify(true, Schemifier.infoF _, Category)
    Schemifier.schemify(true, Schemifier.infoF _, CommentArticle)
    Schemifier.schemify(true, Schemifier.infoF _, GeolocateArticle)
    Schemifier.schemify(true, Schemifier.infoF _, AvvisoFile)
    Schemifier.schemify(true, Schemifier.infoF _, FileArticle)
    Schemifier.schemify(true, Schemifier.infoF _, Advert)
    Schemifier.schemify(true, Schemifier.infoF _, AvvisoOption)
    
    //add default values on the DB
    DefaultAvvisoOption.save
    DefaultLanguage.save    
    DefaultCategory.save
    DefaultLocalization.save
    DefaultUser.save

    // where to search snippet
    LiftRules.addToPackages("code")

    LiftRules.dispatch.append(ArticleRest)
    LiftRules.dispatch.append(AvvisoRest)
    StatelessJson.init()
    LiftRules.noticesAutoFadeOut.default.set((noticeType: NoticeType.Value) => Full((1 seconds, 2 seconds)))

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.

    LiftRules.setSiteMap(Site.siteMap)

    //Init the jQuery module, see http://liftweb.net/jquery for more information.
    LiftRules.jsArtifacts = JQueryArtifacts
    JQueryModule.InitParam.JQuery = JQueryModule.JQuery172
    JQueryModule.init()

    //Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }
}
