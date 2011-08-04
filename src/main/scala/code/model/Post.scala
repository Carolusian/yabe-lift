package code.model

import net.liftweb.mapper._

class Post extends LongKeyedMapper[Post] with IdPK {
  def getSingleton = Post

  object title extends MappedString(this, 100)
  object content extends MappedTextarea(this, 65535)
}

object Post extends Post with LongKeyedMetaMapper[Post] with CRUDify[Long, Post]