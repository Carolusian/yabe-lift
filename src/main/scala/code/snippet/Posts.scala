package code.snippet

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.mapper._
import java.util.Date
import Helpers._
import code.model._
import code.lib._
import MapperBinder._

class Posts {
  def listLatest: CssSel = {
    val latestPost = Post.find(OrderBy(Post.id, Descending))
    //"*" #> bindMapper()

    latestPost match {
      case Full(p) => {
        "*" #> bindMapper(p,{
          ".post-comments *" #> (" | " + p.countComments + " comments " + p.latestCommentAuthor)
        }) _
      }
      case _ => "*" #> <span></span>
    }
  }

  def listOlder: CssSel = {
    val latestPost = Post.find(OrderBy(Post.id, Descending))

    latestPost match {
      case Full(p) => {
        val olderPosts = Post.findAll(OrderBy(Post.id, Descending)).
          filter(p.id != _.id)

        renderPostsList(olderPosts)
      }
      case _ => "*" #> ""
    }
  }

  def listPostsByTag:CssSel = {
    val posts = Post.getPostsByTag(S.param("tag") openOr "")

    renderPostsList(posts)
  }

  def getTag:CssSel = {
    "span" #> (S.param("tag") openOr "")
  }

  def titlePostsByTag:CssSel = {
    "*" #> <title>{("Posts tagged with " + (S.param("tag") openOr ""))}</title>
  }

  private def renderPostsList(posts:List[Post]):CssSel = {
   "*" #> posts.map {
    p =>
      "*" #> bindMapper(p,{
        ".post-comments *" #> (" | " + p.countComments + " comments " + p.latestCommentAuthor)
      }) _
    }
  }

  def title:CssSel = {
    val post = Post.find(By(Post.id, S.param("id").openTheBox.toLong))
    post match {
      case Full(p) => "*" #> <title>{p.title}</title>
      case _ => "*" #> <title>invalid post</title>
    }
  }

  def read: CssSel = {
    val post = Post.find(By(Post.id, S.param("id").openTheBox.toLong))

    post match {
      case Full(p) => {
        "*" #> bindMapper(p, {".post-tags *" #> Unparsed(p.showTagMetaStr) }) _
      }
      case _ => "*" #> ""
    }
  }

  def prev: CssSel = {
    val currentPostId = S.param("id").openTheBox.toLong
    val prevPost =
      Post.find(OrderBy(Post.id, Descending),
        By_<(Post.id, currentPostId))

    prevPost match {
      case Full(p) => {
        "a [href]" #> ("/read/" + p.id.get) &
          "a *" #> p.title.get
      }
      case _ => "*" #> ""
    }
  }

  def next: CssSel = {
    val currentPostId = S.param("id").openTheBox.toLong
    val nextPost =
      Post.find(OrderBy(Post.id, Ascending),
        By_>(Post.id, currentPostId))

    nextPost match {
      case Full(p) => {
        "a [href]" #> ("/read/" + p.id.get) &
          "a *" #> p.title.get
      }
      case _ => "*" #> ""
    }
  }

  //Count the number of posts that posted by current user
  def countByUser: CssSel = {
    val userId = User.currentUserId.openTheBox
    val count = Post.count(By(Post.author, userId.toLong))
    "span" #> count
  }

  //list posts posted by this user
  def listByUser: CssSel = {
    val userId = User.currentUserId.openTheBox
    val posts = Post.findAll(By(Post.author, userId.toLong))

    var odd = "even"
    "*" #> posts.map {
      post =>
        odd = YabeHelper.oddOrEven(odd)

        "p [class]" #> ("post " + odd) &
          "a [href]" #> ("/admin/posts/edit/" + post.id) &
          "a *" #> post.title
    }
  }

  def add: CssSel = {
    val post = Post.create

    def process() = {
      post.author.set(User.currentUserId.openTheBox.toInt)
      post.postedAt.set(new Date())
      post.validate match {
        case Nil => {
          post.save
          post.tags.setMultiple(post, S.param("tags").openTheBox)
          S.redirectTo("/admin/posts/index")
        }
        case errors => S.error(errors)
      }
    }

    "#post-add" #> bindMapper(post) _ &
      "type=submit" #> SHtml.onSubmitUnit(() => process)
  }

  def edit: CssSel = {
    val id = S.param("id").openTheBox
    val post = Post.find(By(Post.id, id.toLong)).openTheBox

    def process() = {
      post.validate match {
        case Nil => {
          post.save
          S.redirectTo("/admin/posts/index")
        }
        case errors => S.error(errors)
      }
    }

    if (post.author.toLong != User.currentUserId.openTheBox.toLong) {
      "*" #> <span>Sorry, you do not have permission to edit this post in this page.</span>
    } else {
      "#post-edit" #> bindMapper(post) _ &
        "name=tags" #> SHtml.text(post.tags.concat, post.tags.setMultiple(post, _)) &
        "type=submit" #> SHtml.onSubmitUnit(process)
    }
  }

  def getUserName: CssSel = {
    val firstName = User.currentUser.openTheBox.firstName
    val lastName = User.currentUser.openTheBox.lastName

    "span" #> (firstName + " " + lastName)
  }
}