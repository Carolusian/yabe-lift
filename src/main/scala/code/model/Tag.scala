package code.model

import net.liftweb.mapper._

class Tag extends LongKeyedMapper[Tag] with IdPK {

  def getSingleton = Tag

  object name extends MappedString(this, 50)
  object posts extends  HasManyThrough(this, Post, PostTag, PostTag.tag, PostTag.post)
}

object Tag extends Tag with LongKeyedMetaMapper[Tag] with CRUDify[Long, Tag]