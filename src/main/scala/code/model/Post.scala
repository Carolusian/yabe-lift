package code.model

import net.liftweb.mapper._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import scala.Predef._

class Post extends LongKeyedMapper[Post] with IdPK {
  def getSingleton = Post

  object author extends MappedLongForeignKey(this, User) {
    override def validSelectValues = {
      val users = User.findAll().map(x => (x.id.get, x.email.get))
      val list = (0.toLong, "(Please select a user)") :: users
      Full(list)
    }

    override def validations = {
      def needAuthor(author: Long) = {
        if (author == 0) List(FieldError(this, "Please select a user."))
        else List[FieldError]()
      }

      needAuthor _ :: Nil
    }

    def getAuthor = {
      User.find(By(User.id, author.get)).openTheBox
    }
  }

  object title extends MappedString(this, 140) {
    override def validations = {
      valMinLen(1, "Please input title.") _ :: Nil
    }
  }

  object content extends MappedText(this) {
    override def validations = {
      def notNull(txt: String) = {
        if (txt == "")
          List(FieldError(this, "Please input content."))
        else
          List[FieldError]()
      }

      notNull _ :: Nil
    }
  }

  object postedAt extends MappedDate(this) {
    override def validations = {
      def isDate(txt: java.util.Date) = {
        if (txt == null)
          List(FieldError(this, "Please input a validate date."))
        else
          List[FieldError]()
      }

      isDate _ :: Nil
    }

    override def parse(s: String): Box[java.util.Date] = {
      val df = new java.text.SimpleDateFormat("yyyy-MM-dd")
      try {
        val date = df.parse(s)
        Full(date)
      } catch {
        case _ => Empty
      }
    }
  }

  object tags extends HasManyThrough(this, Post, PostTag, PostTag.tag, PostTag.post) {
    def setMultiple(post: Post, tags: String) {
      //delete old tag relation
      PostTag.findAll(By(PostTag.post, post.id)).foreach(_.delete_!)

      //set new tag relation
      Tag.createAndGetTags(splitTags(tags)).distinct.foreach(PostTag.join(post, _))
    }

    //Format tags as "tag1 tag2 tag3.."
    def concat: String = {
      val postTags = PostTag
        .findAll(By(PostTag.post, Post.this.id))

      postTags match {
        case Nil => ""
        case _ => postTags
          .map(pt => Tag.getName(pt.tag.get))
          .reduceLeft(_ + " " + _)
      }
    }

    private def splitTags(tags: String) = {
      tags.split(" ").toList
    }
  }

  //delete its comments first
  override def delete_!(): Boolean = {
    val comments = Comment.findAll(By(Comment.post, this.id.get))
    comments.foreach(_.delete_!)

    super.delete_!
  }
}

object Post extends Post with LongKeyedMetaMapper[Post] with CRUDify[Long, Post]
