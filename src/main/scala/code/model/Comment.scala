package code.model

import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util._

class Comment extends LongKeyedMapper[Comment] with IdPK {
	def getSingleton = Comment

	object author extends MappedString(this,140)
	
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
	  
	  def short = {
	    this.get.length match {
	      case l if l > 50 => this.get.substring(50) + "..."
	      case _ => this.get
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
	  
	  def getTitle = {
	    val post = Post.find(By(Post.id,this.get))
	    post match {
	      case Full(p) => p.title.get
	      case _ => ""
	    }
	  }
	}
}

object Comment extends Comment with LongKeyedMetaMapper[Comment] with CRUDify[Long,Comment]
