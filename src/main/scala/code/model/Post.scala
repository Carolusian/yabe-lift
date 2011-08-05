package code.model

import net.liftweb.mapper._

class Post extends LongKeyedMapper[Post] with IdPK {
  def getSingleton = Post

  object title extends MappedString(this, 140)
  object content extends MappedTextarea(this, 65535)
  object postedAt extends MappedDate(this)

  object tags extends HasManyThrough(this, Tag, PostTag, PostTag.post, PostTag.tag)
}

object Post extends Post with LongKeyedMetaMapper[Post] with CRUDify[Long, Post]