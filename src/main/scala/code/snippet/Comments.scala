package code.snippet

import scala.xml.Unparsed
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import js._
import JsCmds._
import JE._
import java.util.Date
import Helpers._
import code.model._
import code.lib._
import code.comet._

class Comments {

  object postId extends RequestVar(S.param("id").openTheBox.toLong)

  def add = {
    var comment = Comment.create
    var captchaCode = ""

    //User curry function to keep postId, otherwise, postId will be lost in ajax request
    def process(id: Long)(): JsCmd = {
      comment.postedAt.set(new java.util.Date)
      comment.post.set(id)

      if ((S.getSessionAttribute("captcha") openOr "") != captchaCode) {
        JE.Call("clearError") &
          JE.Call("showError", Str("Captcha is not correct."))
      } else {
        comment.validate match {
          case Nil => {
            comment.save
            //prepare for another comment
            comment = Comment.create

            CommentsServer ! id

            JE.Call("clearError") & JE.Call("clearForm")
          }
          case errors => S.error(errors); JE.Call("clearError") & JE.Call("showError", Str(errors.head.msg.toString))
        }
      }
    }

    "name=author" #> SHtml.text(comment.author.get, comment.author.set(_)) &
      "name=code" #> SHtml.text(captchaCode, captchaCode = _) &
      "name=content" #> (SHtml.textarea(comment.content.get, comment.content.set(_), "id" -> "content") ++
        SHtml.hidden(process(postId.is)))
  }

  def initComet = {
    CommentsServer ! postId.is
    "*" #> ""
  }

  /******************************************************************************************
   * For admin panel..
   *****************************************************************************************/
  private object searchStr extends RequestVar("")

  def list: CssSel = {
    val comments = getComments()
    var odd = "even"
    "tr" #> comments.map {
      c =>
        odd = YabeHelper.oddOrEven(odd);
        "tr [class]" #> odd &
          "a [href]" #> ("/admin/comments/edit/" + c.id) &
          "a *" #> c.content.short &
          ".post-title" #> (c.post.getTitle) &
          ".comment-author" #> (c.author.get match {
            case "" => "guest"
            case _ => c.author.get
          })
    }
  }

  def delete = {
    val id = S.param("id").openTheBox
    val comment = Comment.find(By(Comment.id, id.toLong)).openTheBox
    def process() = {
      comment.delete_!
      S.redirectTo("/admin/comments/index")
    }
    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def sort = {
    val search = searchStr.is

    if(getCommentsOrder == "DESC")
      "a [class]" #> "crudSortedDesc" &
      "a" #> SHtml.link("/admin/comments/index?order=ASC",
          ()=>searchStr(search),
          <span>Contents</span>,
          "class"->"crudSortedDesc")
    else
      "a [class]" #> "crudSortedAsc" &
      "a" #> SHtml.link("/admin/comments/index?order=DESC",
          ()=>searchStr(search),
          <span>Content</span>,
          "class"->"crudSortedAsc")
  }

  def search:CssSel = {
    "name=search" #> SHtml.textElem(searchStr)
  }

  def count = {
    "span" #> countComments
  }

  private def countComments() = {
    if(validSearch()) {
      Comment.count(BySql(" content like '%"+searchStr.is+"%' ",
              IHaveValidatedThisSQL("charliechen","2011-07-21")))
    } else
      Comment.count()
  }

  private def getComments() = {
    val comments = validSearch() match {
      case x if x == true => Comment.findAll(
        BySql(" content like '%" + searchStr.is + "%'",
          IHaveValidatedThisSQL("charliechen", "2011-07-21")),
        OrderBy(Comment.id, Ascending))

      case _ => Comment.findAll(OrderBy(Comment.content, Ascending))
    }

    getCommentsOrder match {
      case "DESC" => comments.reverse
      case "ASC" => comments
    }
  }

  private def validSearch() = searchStr.is != ""

  private def getCommentsOrder = {
    S.param("order") match {
      case Full(p) if p == "DESC" => "DESC"
      case _ => "ASC"
    }
  }
}

class CommentsEdit extends StatefulSnippet {
  private val id = S.param("id").openTheBox
  private val comment = Comment.find(By(Comment.id, id.toLong)).openTheBox

  def dispatch = {case "render" => render}

  def render = {

    def process() = {
      comment.validate match {
        case Nil => {
          comment.save
          S.redirectTo("/admin/comments/index")
        }
        case errors => S.error(errors)
      }
    }

    "name=author" #> SHtml.text(comment.author, comment.author.set(_)) &
      "name=content" #> SHtml.textarea(comment.content, comment.content.set(_)) &
      "name=postedAt" #> SHtml.text(YabeHelper.fmtDateStr(comment.postedAt), comment.postedAt.setFromAny(_)) &
      "name=post_id" #> comment.post.toForm &
      "type=submit" #> SHtml.onSubmitUnit(() => process)
  }
}