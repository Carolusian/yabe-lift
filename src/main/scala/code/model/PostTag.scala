package code.model

import net.liftweb.mapper._

class PostTag extends LongKeyedMapper[PostTag] with IdPK {
  def getSingleton = PostTag
  object post extends MappedLongForeignKey(this, Post)
  object tag extends MappedLongForeignKey(this,Tag)
}

object PostTag extends PostTag with LongKeyedMetaMapper[PostTag] {
    def join(p:Post, t:Tag) = this.create.tag(t).post(p).save
}