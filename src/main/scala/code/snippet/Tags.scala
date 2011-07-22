package code.snippet

import net.liftweb.http.{S,SHtml, RequestVar}
import net.liftweb.common._
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import code.lib.YabeHelper
import code.model.Tag
import code.model.Post.tags

class Tags {
  private object searchStr extends RequestVar("")

  def search:CssSel = {
    "name=search" #> SHtml.textElem(searchStr)
  }

  def list:CssSel ={
    val tags = getTags
    var odd = "even"

    "tr" #> tags.map {
      t =>
        odd = YabeHelper.oddOrEven(odd);
        "tr [class]" #> odd &
          "a [href]" #> ("/admin/tags/edit/" + t.id) &
          "a *" #> t.name.get
    }
  }

  def add:CssSel = {
    val tag = Tag.create

    def process() = {
      tag.validate match {
        case Nil => {
          tag.save
          S.redirectTo("/admin/tags/index")
        }
        case errors => S.error(errors)
      }
    }

    "name=name" #> SHtml.onSubmit(tag.name.set(_)) &
      "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def edit:CssSel ={
    val tag = Tag.find(By(Tag.id, S.param("id").openTheBox.toLong)).openTheBox

    def process() = {
      tag.validate match {
        case Nil => {
          tag.save
          S.redirectTo("/admin/tags/index")
        }
        case errors => S.error(errors)
      }
    }

    "name=name" #> SHtml.text(tag.name.get, tag.name.set(_)) &
      "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def delete:CssSel = {
    val tag = Tag.find(By(Tag.id, S.param("id").openTheBox.toLong)).openTheBox

    def process() = {
      tag.delete_!
      S.redirectTo("/admin/tags/index")
    }

    "type=submit" #> SHtml.onSubmitUnit(process)
  }

  def sort:CssSel = {
    val search = searchStr.is

    if (getTagsOrder == "DESC")
      "a [class]" #> "crudSortedDesc" &
        "a" #> SHtml.link("/admin/tags/index?order=ASC",
          () => searchStr(search),
          <span>Tags</span>,
          "class" -> "crudSortedDesc")
    else
      "a [class]" #> "crudSortedAsc" &
        "a" #> SHtml.link("/admin/tags/index?order=DESC",
          () => searchStr(search),
          <span>Tags</span>,
          "class" -> "crudSortedAsc")
  }

  def count:CssSel = {
    val count = Tag.count(BySql(" name like '%" + searchStr.is + "%' ",
          IHaveValidatedThisSQL("charliechen", "2011-07-22")))

    "span" #> count
  }

  private def getTags() = {
    val tags = validSearch() match {
      case x if x == true => Tag.findAll(
        BySql(" name like '%" + searchStr.is + "%' ",
          IHaveValidatedThisSQL("charliechen", "2011-07-22")),
        OrderBy(Tag.name, Ascending))

      case _ => Tag.findAll(OrderBy(Tag.name, Ascending))
    }

    getTagsOrder match {
      case "DESC" => tags.reverse
      case "ASC" => tags
    }
  }

  private def validSearch() = searchStr.is != ""

  private def getTagsOrder = {
    S.param("order") match {
      case Full(p) if p == "DESC" => "DESC"
      case _ => "ASC"
    }
  }
}