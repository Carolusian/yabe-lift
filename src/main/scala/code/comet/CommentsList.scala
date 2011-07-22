package code.comet

import scala.xml.Unparsed
import net.liftweb._
import http._
import util._
import Helpers._
import code.model._
import code.lib._

class CommentsList extends CometActor with CometListener {
  private var comments = List[Comment]()
  
  def registerWith = CommentsServer
  
  override def lowPriority = {
    case c:List[Comment] => comments = c; reRender
  }
  
  def render = {
    ".comment" #> comments.map {
      c => 
        ".comment-author *" #> (c.author.get match {
          case "" => "guest"
          case _ => c.author.get
        })  & 
        ".comment-date *" #> YabeHelper.fmtDateStr(c.postedAt.get) &
        ".comment-content-span" #> Unparsed(c.content.get.replaceAll("\n","<br />"))
    }
  }
}