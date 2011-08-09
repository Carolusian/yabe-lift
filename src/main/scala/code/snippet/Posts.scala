package code.snippet

import net.liftweb.http._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._
import scala.xml.Unparsed

import code.model._

class Posts {
  def latestPost:CssSel = {
    val latestPost = Post.find(OrderBy(Post.id, Descending))
    
    latestPost match {
      case Full(p) => {
        "a *" #> p.title.get &
        "a [href]" #> ("/read/" + p.id.get) &
        ".post-author *" #> ("by " + p.author.getFullName) &
        ".post-date *" #> p.postedAt.asHtml &
        ".post-comments *" #> ("| " + p.countComments + " comments " + p.latestCommentAuthor ) &
        ".post-content-span *" #> p.content.get 
      }
      
      case _ => "*" #> <span></span>
    }
  }
  
  def listOlder:CssSel = {
    val latestPost = Post.find(OrderBy(Post.id,Descending))
    
    latestPost match {
      case Full(p) => {
        val olderPosts = Post.findAll(OrderBy(Post.id, Descending)).
          filter(p.id != _.id)

        renderPostsList(olderPosts)
      }
      case _ => "*" #> ""
    }
  }
  
  def read: CssSel = {
    val post = Post.find(By(Post.id, S.param("id").openTheBox.toLong))

    post match {
      case Full(p) => {
         "a *" #> p.title.get &
        "a [href]" #> ("/read/" + p.id.get) &
        ".post-author *" #> ("by " + p.author.getFullName) &
        ".post-date *" #> p.postedAt.asHtml &
        ".post-tags *" #> Unparsed(p.showTagMetaStr) &
        ".post-content-span *" #> p.content.get 
        
      }
      case _ => "*" #> ""
    }
  }
  
  private def renderPostsList(posts:List[Post]):CssSel = {
   "*" #> posts.map {
	  p =>
      "a *" #> p.title.get &
        "a [href]" #> ("/read/" + p.id.get) &
        ".post-author *" #> ("by " + p.author.getFullName) &
        ".post-date *" #> p.postedAt.asHtml &
        ".post-comments *" #> ("| " + p.countComments + " comments " + p.latestCommentAuthor ) 
    }
  }
}