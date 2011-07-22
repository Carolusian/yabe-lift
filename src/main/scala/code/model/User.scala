package code
package model

import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S

/**
 * The singleton that has methods for accessing the database
 */
object User extends User with MetaMegaProtoUser[User] {
  override def dbTableName = "users" // define the DB table name
  //override def screenWrap = Full(<lift:surround with="default" at="content">
	//		       <lift:bind /></lift:surround>)
	
	override def homePage = "/admin/posts/index"
	
	override def loginXhtml ={
		<lift:surround with="login" at="content">
		<h1></h1> 
		<form action={S.uri} method="POST" accept-charset="utf-8">
	 	<lift:msgs showAll="true" />
		<p id="username-field"> 
		<label for="username">Your email:</label> 
		<user:email />
		</p> 
		<p id="password-field"> 
		<label for="password">Your password:</label> 
		<user:password />
		</p> 
		<p id="signin-field"> 
		<user:submit />
		</p> 
		
		</form>
	  </lift:surround>
	}

  // define the order fields will appear in forms and output
  override def fieldOrder = List(id, firstName, lastName, email,
  locale, timezone, password, textArea)

  // comment this line out to require email validations
  override def skipEmailValidation = true 
}

/**
 * An O-R mapped "User" class that includes first name, last name, password and we add a "Personal Essay" to it
 */
class User extends MegaProtoUser[User] {
  def getSingleton = User // what's the "meta" server

  // define an additional field for a personal essay
  object textArea extends MappedTextarea(this, 2048) {
    override def textareaRows  = 10
    override def textareaCols = 50
    override def displayName = "Personal Essay"
  }
  
  //Demo users can not be deleted and modified
  object isDemo extends MappedBoolean(this) {
    override def defaultValue=false
  }

  def hasPosts():Boolean = {
    val postCount = Post.count(By(Post.author, this.id.get))
    postCount match {
      case 0 => false
      case _ => true
    }
  }
}

