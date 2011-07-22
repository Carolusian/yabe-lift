package code.model

import net.liftweb.mapper._
import net.liftweb.common.{Full, Empty}
import net.liftweb.util.FieldError
import code.model.Post.tags

class Tag extends LongKeyedMapper[Tag] with IdPK {
  def getSingleton = Tag

  object name extends MappedString(this, 50) {
    override def validations = {
      //Make there is no duplicate tags
      def duplicate(n: String): List[FieldError] = {
        val count = Tag.count(By(Tag.name, n), NotBy(Tag.id, Tag.this.id))
        count match {
          case 0 => List[FieldError]()
          case _ => List(FieldError(this, "Tags can not be duplicated."))
        }
      }

      def hasSpace(n: String): List[FieldError] = {
        if (n.contains(" "))
          List(FieldError(this, "Tag should not have spaces."))
        else
          List[FieldError]()
      }

      valMinLen(1, "Please input tag.") _ :: hasSpace _ :: duplicate _ :: Nil
    }
  }

  object posts extends HasManyThrough(this, Post, PostTag, PostTag.post, PostTag.tag)

  //delete its comments first
  override def delete_!(): Boolean = {
    val postTags = PostTag.findAll(By(PostTag.tag, this.id.get))
    postTags.foreach(_.delete_!)

    super.delete_!
  }
}

object Tag extends Tag with LongKeyedMetaMapper[Tag] with CRUDify[Long, Tag] {
  def createAndGetTags(tags: List[String]): List[Tag] = {
    tags.filter(t=>t.contains(" ") == false && t.length > 0).map {
      t =>
        Tag.find(By(Tag.name, t)) match {
          case Full(tag) => tag
          case _ => createAndGet(t)
        }
    }
  }

  def getName(id: Long) = {
    val tag = Tag.find(By(Tag.id, id))

    tag match {
      case Empty => ""
      case _ => tag.openTheBox.name.get
    }
  }

  def createAndGet(tag: String) = {
    val newTag = Tag.create
    newTag.name(tag.trim)
    newTag.save
    newTag
  }
}