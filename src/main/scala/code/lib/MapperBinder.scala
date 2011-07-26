package code.lib

import net.liftweb.mapper._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import xml.{Group, Elem, NodeSeq, Text, Node}
import code.model.Post
import xml.transform.{RuleTransformer, RewriteRule}

trait MapperBinder {
  def bindMapper(data: Post)(in: NodeSeq): NodeSeq = {
    val tranformShow = new RewriteRule {
      override def transform(n:Node):NodeSeq = n match {
        //check if text has @fieldName
        case e:Elem  => {
          println(e + "|" + e.text)
          new Elem(e.prefix,
            e.label,
            e.attributes,
            e.scope,
            data.title.asHtml)
        }


        case n => n
      }

      //check if it has sign '@'
      def hasAt(s:String):Boolean = {
        //regex for @fieldName
        val r = """\s*\@[a-zA-Z0-9\_]+\s*""".r
        val res = r.findAllIn(s)

        if(res.length > 0 )
          true
        else
          false
      }
    }

    new RuleTransformer(tranformShow).transform(in)
  }



}

object MapperBinder extends MapperBinder