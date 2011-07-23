package bootstrap.liftweb

import net.liftweb._
import net.liftweb.http._
import util._
import Helpers._

import common._
import http._
import sitemap._
import Loc._
import mapper._

import code.model._
import code.lib._

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
    Schemifier.schemify(true, Schemifier.infoF _, User, Post, Comment, Tag, PostTag)

    // where to search snippet
    LiftRules.addToPackages("code")

    val IfUserLoggedIn = If(() => User.loggedIn_?,
      () => RedirectResponse("/login"))
    val IfAdminLoggedIn = If(() => User.loggedIn_? && User.superUser_?,
      () => RedirectResponse("/admin/posts/index"))

    def menus = List(
      Menu.i("Home") / "index", //>> User.AddUserMenusAfter,
      Menu.i("Read") / "read",
      Menu.i("Posts by tag") / "posts",
      //Can be accessed by both users and admins
      Menu.i("My posts") / "admin" / "posts" / ** >> IfUserLoggedIn >> LocGroup("admin"),

      //Can be accessed by admins
      Menu.i("Posts") / "admin" / "all_posts" / ** >> IfAdminLoggedIn >> LocGroup("admin"),
      Menu.i("Tags") / "admin" / "tags" / ** >> IfAdminLoggedIn >> LocGroup("admin"),
      Menu.i("Comments") / "admin" / "comments" / ** >> IfAdminLoggedIn >> LocGroup("admin"),
      Menu.i("Users") / "admin" / "users" / ** >> IfAdminLoggedIn >> LocGroup("admin")
    )

    // Build SiteMap
    def sitemap = SiteMap(
      menus: _*
    )

    def sitemapMutators = User.sitemapMutator

    // set the sitemap.  Note if you don't want access control for
    // each page, just comment this line out.
    LiftRules.setSiteMapFunc(() => sitemapMutators(sitemap))

    //Rewrite
    LiftRules.statelessRewrite.append {
      //Login and logout
      case RewriteRequest(ParsePath("login" :: Nil, _, _, _), _, _) =>
        RewriteResponse("user_mgt" :: "login" :: Nil)
      case RewriteRequest(ParsePath("logout" :: Nil, _, _, _), _, _) =>
        RewriteResponse("user_mgt" :: "logout" :: Nil)

      //Edit users
      case RewriteRequest(ParsePath("admin" :: "users" :: "edit" :: id :: Nil, _, _, _), _, _) =>
        RewriteResponse("admin" :: "users" :: "edit" :: Nil, Map("id" -> id))

      //Edit posts
      case RewriteRequest(ParsePath("admin" :: "posts" :: "edit" :: id :: Nil, _, _, _), _, _) =>
        RewriteResponse("admin" :: "posts" :: "edit" :: Nil, Map("id" -> id))
      case RewriteRequest(ParsePath("admin" :: "all_posts" :: "edit" :: id :: Nil, _, _, _), _, _) =>
        RewriteResponse("admin" :: "all_posts" :: "edit" :: Nil, Map("id" -> id))

      //edit comment
      case RewriteRequest(ParsePath("admin" :: "comments" :: "edit" :: id :: Nil, _, _, _), _, _) =>
        RewriteResponse("admin" :: "comments" :: "edit" :: Nil, Map("id" -> id))

      //edit tag
      case RewriteRequest(ParsePath("admin" :: "tags" :: "edit" :: id :: Nil,_,_,_),_,_) =>
        RewriteResponse("admin" :: "tags" :: "edit" :: Nil, Map("id" -> id))

      //read post
      case RewriteRequest(ParsePath("read" :: id :: Nil, _, _, _), _, _) =>
        RewriteResponse("read" :: Nil, Map("id" -> id))

      //list posts by tag
      case RewriteRequest(ParsePath("posts" :: tag :: Nil, _, _, _), _, _) =>
        RewriteResponse("posts":: Nil, Map("tag" -> tag))
    }

    //Captcha function
    LiftRules.dispatch.append {
      case Req("captcha" :: Nil, _, _) => YabeHelper.captcha
    }

    //Create Demo Users
    initUsers()

    // Use jQuery 1.4
    //LiftRules.jsArtifacts = net.liftweb.http.js.jquery.JQuery14Artifacts

    //Show the spinny image when an Ajax call starts
    //LiftRules.ajaxStart =
    //  Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    // Make the spinny image go away when it ends
    //LiftRules.ajaxEnd =
    //  Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // What is the function to test if a user is logged in?
    //LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    // Use HTML5 for rendering
    LiftRules.htmlProperties.default.set((r: Req) =>
      new Html5Properties(r.userAgent))

    // Make a transaction span the whole HTTP request
    S.addAround(DB.buildLoanWrapper)
  }

  //Init user data, one super user and one normal user.
  def initUsers() {
    if (isDemoSuperUserExist == false) {
      val superUser = User.create
        .firstName("Super")
        .lastName("Demo")
        .email("super@demo.com")
        .password("demouser")
        .superUser(true)
        .isDemo(true)
        .validated(true)

      superUser.save
    }

    if (isDemoNormalUserExist == false) {
      val normalUser = User.create
        .firstName("Normal")
        .lastName("Demo")
        .email("normal@demo.com")
        .password("demouser")
        .superUser(false)
        .isDemo(true)
        .validated(true)

      normalUser.save
    }
  }

  def isDemoSuperUserExist() = {
    User.count(By(User.isDemo, true), By(User.superUser, true)) match {
      case x if x > 0 => true
      case _ => false
    }
  }

  def isDemoNormalUserExist() = {
    User.count(By(User.isDemo, true), NotBy(User.superUser, true)) match {
      case x if x > 0 => true
      case _ => false
    }
  }
}
