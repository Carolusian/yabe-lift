package code.model

import net.liftweb.mapper._
import net.liftweb.common.Full

class Post extends LongKeyedMapper[Post] with IdPK {
  def getSingleton = Post

  object author extends MappedLongForeignKey(this, User) {
	override def validSelectValues = {
	  Full(User.findAll.map(u=>(u.id.get,u.firstName.get + " " + u.lastName.get)))
	}
	
	def getFullName:String = {
	  val u = User.find(By(User.id, this.get))
	  u match {
	    case Full(u) => u.firstName.get + " " + u.lastName.get
	    case _ => "unknown"
	  }
	}
  }
  object title extends MappedString(this, 140)
  object content extends MappedTextarea(this, 65535)
  object postedAt extends MappedDate(this)

  object tags extends HasManyThrough(this, Tag, PostTag, PostTag.post, PostTag.tag)
  
  def countComments:Long = {
    Comment.count(By(Comment.post, this.id.get))
  }
  
  def latestCommentAuthor:String = {
    if(countComments <= 0)
      ""
    else {
      val latest = Comment.find(By(Comment.post,this.id),
        OrderBy(Comment.postedAt, Descending)).openTheBox

      latest.author match {
        case author if (author.length > 0) => ", lastest by "+latest.author
        case _ => ", lastest by guest"
      }
    }
  }
  
  def showTagMetaStr:String = {
    val postTags = PostTag.findAll(By(PostTag.post, this.id))
    val tagNames = postTags.map { pt =>
      val tag = Tag.find(By(Tag.id,pt.tag)).openTheBox
      "<a href='/posts/"+tag.name.get+"'>"+tag.name.get+"</a>"
    }
    if(tagNames.length > 0)
     " - tagged: " + tagNames.reduceLeft(_ + ", "+ _)
    else
      " - no tags"
  }
}

object Post extends Post with LongKeyedMetaMapper[Post] with CRUDify[Long, Post]