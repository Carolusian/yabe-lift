package code.snippet

import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util._
import net.liftweb.mapper.OrderBy
import net.liftweb.mapper.Descending
import xml.NodeSeq

import code.lib._
import MapperBinder._
import code.model.Post


class Test {
  def index:CssSel = {
    val post = Post.find(OrderBy(Post.id, Descending)).openTheBox

    "*" #> { bindMapper(post) _ }
  }
}