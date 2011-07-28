package code.model

import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util._
import scala.xml.{NodeSeq,Text,Node}

class Comment extends LongKeyedMapper[Comment] with IdPK {
	def getSingleton = Comment

	object author extends MappedString(this,140) {
    override def asHtml ={
      this.get match {
        case "" => Text("guest")
        case _ => Text(this.get)
      }
    }
  }
	
	object content extends MappedText(this) {
	  override def validations = {
		def notNull(txt:String ) = {
		  if(txt=="")
		    List(FieldError(this,"Please input content."))
		  else
		    List[FieldError]()
		}
	    
		notNull _ :: Nil
	  }
	  
	  def short:Node = {
	    this.get.length match {
	      case l if l > 50 => Text(this.get.substring(50) + "...")
	      case _ => Text(this.get)
	    }
	  }
	}
	
	object postedAt extends MappedDateTime(this) {
    override def validations = {
	    def isDate(txt:java.util.Date) = {
	      if(txt==null)
	        List(FieldError(this,"Please input a validate date."))
	      else
	        List[FieldError]()
	    }

	    isDate _ :: Nil
	  }

    override def parse(s:String):Box[java.util.Date] = {
	    val df = new java.text.SimpleDateFormat("yyyy-MM-dd")
	    try {
	      val date = df.parse(s)
	      Full(date)
	    } catch {
	      case _ => Empty
	    }
	  }
  }
	
	object post extends MappedLongForeignKey(this,Post) {

    override def validSelectValues = {
	    val posts = Post.findAll().map(x=>(x.id.get,x.title.get))
	    val list = (0.toLong,"(Please select a post)")::posts
	    Full(list)
	  }

	  override def validations = {
	    def validatePost(id:Long) =  {
	      val posts = Post.findAll(By(Post.id, id))
	      posts match {
	        case Nil => List(FieldError(this,"Please add comments to valid posts."))
	        case _ => List[FieldError]()
	      }
	    }
	    
	    validatePost _ :: Nil
	  }

    override def asHtml = {
	    val post = Post.find(By(Post.id,this.get))
	    post match {
	      case Full(p) => Text(p.title.get)
	      case _ => Text("")
	    }
    }
	}
}

object Comment extends Comment with LongKeyedMetaMapper[Comment] with CRUDify[Long,Comment]
