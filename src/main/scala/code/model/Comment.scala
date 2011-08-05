package code.model

import net.liftweb.mapper._
import net.liftweb.common.Full

class Comment extends LongKeyedMapper[Comment] with IdPK {
  def getSingleton = Comment

  object author extends MappedString(this, 50)
  object content extends MappedTextarea(this, 1000)
  object postedAt extends MappedDate(this)
  object post extends MappedLongForeignKey(this, Post) {
    override def validSelectValues = {
      Full(Post.findAll().map(p => (p.id.get, p.title.get)))
    }
  }
}

object Comment extends  Comment with LongKeyedMetaMapper[Comment] with CRUDify[Long, Comment]