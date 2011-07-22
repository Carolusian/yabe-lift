package code.snippet

import scala.xml.{ NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import code.model._
import Helpers._

class AutoLogin {	
  def superUser:CssSel = {
	def process() = {
	  val u = User.findAll(By(User.superUser,true),By(User.isDemo,true))
	  if(u.length>0)
	    User.logUserIn(u.head, ()=>S.redirectTo("/admin/posts/index"))
	}
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
  
  def normalUser:CssSel = {
    def process() = {
	  val u = User.findAll(NotBy(User.superUser,true),By(User.isDemo,true))
	  if(u.length>0)
	    User.logUserIn(u.head, ()=>S.redirectTo("/admin/posts/index"))
	}
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}
