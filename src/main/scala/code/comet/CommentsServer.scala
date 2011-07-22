package code.comet

import net.liftweb._
import http._
import actor._
import net.liftweb.mapper._
import code.model._

object  CommentsServer extends LiftActor with ListenerManager {
  private var comments = List[Comment]()
  
  def createUpdate = comments
  
  override def lowPriority = {
    case postId:Long => {
      comments = Comment.findAll(By(Comment.post,postId))
      updateListeners()
    }
  }
}