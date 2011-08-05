package code.model

import net.liftweb.mapper._
import net.liftweb.common.Full

class PostTag extends LongKeyedMapper[PostTag] with IdPK {

  def getSingleton = PostTag

  object post extends MappedLongForeignKey(this, Post) {
    override def validSelectValues = {
      Full(Post.findAll().map(p=>(p.id.get, p.title.get)))
    }
  }
  object tag extends MappedLongForeignKey(this, Tag) {
    override def validSelectValues = {
      Full(Tag.findAll().map(t=>(t.id.get, t.name.get)))
    }
  }
}

object PostTag extends PostTag with LongKeyedMetaMapper[PostTag] with CRUDify[Long, PostTag]