package code.snippet

import net.liftweb.util._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.mapper._
import Helpers._
import code.model._
import code.lib._
import net.liftweb.mapper.By._

class AllPosts {

  private object searchStr extends RequestVar("")

  def list: CssSel = {
    val posts = getPosts()
    var odd = "even"
    "tr" #> posts.map {
      p =>
        odd = YabeHelper.oddOrEven(odd);
        "tr [class]" #> odd &
          "a [href]" #> ("/admin/all_posts/edit/" + p.id) &
          "a *" #> p.title
    }
  }

  def delete: CssSel = {
    val id = S.param("id").openTheBox
    val post = Post.find(By(Post.id, id.toLong)).openTheBox

    def process() = {
      post.delete_!
      S.redirectTo("/admin/all_posts/index")
    }

    "type=submit" #> SHtml.onSubmitUnit(process)
  }


  def search: CssSel = {
    "name=search" #> SHtml.textElem(searchStr)
  }

  def count = {
    "span" #> countPosts
  }

  def sort = {
    val search = searchStr.is

    if (getPostsOrder == "DESC")
      "a [class]" #> "crudSortedDesc" &
        "a" #> SHtml.link("/admin/all_posts/index?order=ASC",
          () => searchStr(search),
          <span>Posts</span>,
          "class" -> "crudSortedDesc")
    else
      "a [class]" #> "crudSortedAsc" &
        "a" #> SHtml.link("/admin/all_posts/index?order=DESC",
          () => searchStr(search),
          <span>Posts</span>,
          "class" -> "crudSortedAsc")
  }

  private def countPosts() = {
    if (validSearch()) {
      Post.count(BySql(" title like '%" + searchStr.is + "%' or content like '%" + searchStr.is + "%'",
        IHaveValidatedThisSQL("charliechen", "2011-07-11")))
    } else
      Post.count()
  }

  private def getPosts() = {
    val posts = validSearch() match {
      case x if x == true => Post.findAll(
        BySql(" title like '%" + searchStr.is + "%' or content like '%" + searchStr.is + "%'",
          IHaveValidatedThisSQL("charliechen", "2011-07-11")),
        OrderBy(Post.title, Ascending))

      case _ => Post.findAll(OrderBy(Post.title, Ascending))
    }

    getPostsOrder match {
      case "DESC" => posts.reverse
      case "ASC" => posts
    }
  }

  private def validSearch() = searchStr.is != ""


  private def getPostsOrder = {
    S.param("order") match {
      case Full(p) if p == "DESC" => "DESC"
      case _ => "ASC"
    }
  }
}

class AllPostsAdd extends StatefulSnippet {

  val post = Post.create

  def dispatch = {
    case "render" => render
  }

  def render = {
    def process() = {
      post.validate match {
       case Nil => {
          post.save
          post.tags.setMultiple(post, S.param("tags_name_list").openTheBox)
          S.redirectTo("/admin/all_posts/index")
        }
        case errors => S.error(errors)
      }
    }

    "name=title" #> SHtml.text(post.title, post.title.set(_)) &
      "name=content" #> SHtml.textarea(post.content, post.content.set(_)) &
      "name=postedAt" #> SHtml.text(YabeHelper.fmtDateStr(post.postedAt), post.postedAt.setFromAny(_)) &
      "name=author_id" #> post.author.toForm &
      renderTags(PostTag.findAll(By(PostTag.post, post.id)).map(_.tag.get)) &
      "type=submit" #> SHtml.onSubmitUnit(() => process)
  }
}

class AllPostsEdit extends StatefulSnippet {
  private val id = S.param("id").openTheBox
  private val post = Post.find(By(Post.id, id.toLong)).openTheBox

  def dispatch = {
    case "render" => render
  }

  def render: CssSel = {

    def process() = {
      post.validate match {
        case Nil => {
          post.save
          post.tags.setMultiple(post, S.param("tags_name_list").openTheBox)
          S.redirectTo("/admin/all_posts/index")
        }
        case errors => S.error(errors)
      }
    }

    "name=title" #> SHtml.text(post.title, post.title.set(_)) &
      "name=content" #> SHtml.textarea(post.content, post.content.set(_)) &
      "name=postedAt" #> SHtml.text(YabeHelper.fmtDateStr(post.postedAt), post.postedAt.setFromAny(_)) &
      "name=author_id" #> post.author.toForm &
      renderTags(PostTag.findAll(By(PostTag.post, post.id)).map(_.tag.get)) &
      "type=submit" #> SHtml.onSubmitUnit(() => process)
  }
}

object renderTags {
  def apply(selectedTags:List[Long]): CssSel = {
    val tags = Tag.findAll
    var index = 0
    ".tags-list-item" #> tags.map {
      t =>
        index = index + 1
        val selectedName = selectedTags.contains(t.id.get) match {
          case true => t.name.get
          case false => ""
        }
        val selected = selectedName match {
          case "" => ""
          case _ => "selected"
        }
        ".tag [id]" #> index &
        ".tag *" #> t.name.get &
        ".tag [class]" #> ("tag "+selected) &
        "type=hidden [id]" #> ("h"+index) &
        "type=hidden [value]" #> selectedName
    }
  }
}