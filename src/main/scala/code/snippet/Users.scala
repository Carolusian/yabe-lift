package code.snippet

import scala.xml.{ NodeSeq, Text}
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import code.model._
import Helpers._
import code.lib._

class Users {
 
  //Use RequestVar to keep status, 
  //so that change the order of User list can still keep the search result
  private object searchStr extends RequestVar("")
  
  def list:CssSel = {        
    val users = getUsers()
    var odd = "even"
    "#users" #> users.map{
      u => 
      odd=YabeHelper.oddOrEven(odd);
      "tr [class]" #> odd &
      "a [href]" #> ("/admin/users/edit/"+u.id.toString) &
      "a *" #> u.email
    } 
  }
  
  def search:CssSel = {
    "name=search" #> SHtml.textElem(searchStr)
  }
    
  def delete:CssSel = {
    val id = S.param("id").openTheBox
    val user = User.find(By(User.id,id.toLong)).openTheBox
    
    def process() = {
      if(user.isDemo.get==true) {
        S.error("demo_user_error","Demo users can not be deleted")
      } else if(user.hasPosts == true) {
        S.error("has_posts","You need to delete this user's posts first")
      } else {
    	user.delete_!
      	S.redirectTo("/admin/users/")
      }
    }
    
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
  
  def sort = {
    val search = searchStr.is
    
    if(getUserOrder == "DESC")
      "a [class]" #> "crudSortedDesc" &
      "a" #> SHtml.link("/admin/users/index?order=ASC", 
          ()=>searchStr(search), 
          <span>Users</span>,
          "class"->"crudSortedDesc")
    else 
      "a [class]" #> "crudSortedAsc" &
      "a" #> SHtml.link("/admin/users/index?order=DESC", 
          ()=>searchStr(search), 
          <span>Users</span>,
          "class"->"crudSortedAsc")
  }
  
  def count:CssSel = {
    "span" #> countUsers
  }
  
  private def getUsers() = {
    val users = validSearch() match {
      case x if x==true => User.findAll(Like(User.email,"%"+searchStr.is+"%"),
          OrderBy(User.email,Ascending))
          
      case _ => User.findAll(OrderBy(User.email,Ascending))
    }
    
    getUserOrder match {
      case "DESC" => users.reverse
      case "ASC" => users
    }
  }
  
  private def countUsers() = {
    if(validSearch()) {
      User.count(Like(User.email,"%"+searchStr.is+"%"))
    } else
      User.count()
  }
  
  private def getUserOrder = {
    S.param("order") match {
      case Full(p) if p=="DESC" => "DESC"
      case _ => "ASC"
    }
  }
  
  private def validSearch() = searchStr.is!=""
}

//Use stateful snippet in user adding function to keep form values
class UsersAdd extends StatefulSnippet {
  
  private val user = User.create
  
  def dispatch = {case "render"=>render}
 
  def render:CssSel = {
    def process()= {
      user.validate match {
		case Nil => {
		  user.validated.set(true)
		  user.save
		  S.redirectTo("/admin/users/")
		}
		case errors => S.error(errors)
	  }
    }
   	
    "#email" #> SHtml.text(user.email,user.email.set(_)) &
    "#password" #> SHtml.password("",user.password.set(_)) &
    "#firstname" #> SHtml.text(user.firstName, user.firstName.set(_)) &
    "#lastname" #> SHtml.text(user.lastName, user.lastName.set(_)) &
    "#isAdmin" #> SHtml.checkbox(user.superUser, user.superUser.set(_)) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}

class UsersEdit extends StatefulSnippet {
  private val id = S.param("id").openTheBox
  private val user = User.find(By(User.id,id.toLong)).openTheBox
  
  def dispatch = {case "render"=>render}
  
  def render:CssSel = {
    
    def process() = {
      if(user.isDemo.get == true) {
        S.error("demo_user_error","Demo users can not be modified")
        S.redirectTo("/admin/users/edit/"+id.toString)
      }
      user.validate match {
        case Nil => {
          user.save
          S.redirectTo("/admin/users/")
        }
        case errors => S.error(errors)
      }
    }
    
    "#email" #> SHtml.text(user.email,user.email.set(_)) &
    "#password" #> SHtml.password(user.password,user.password.set(_)) & 
    "#firstname" #> SHtml.text(user.firstName,user.firstName.set(_)) &
    "#lastname" #> SHtml.text(user.lastName,user.lastName.set(_)) &
    "#isAdmin" #> SHtml.checkbox(user.superUser, user.superUser.set(_)) &
    "type=submit" #> SHtml.onSubmitUnit(process)
  }
}