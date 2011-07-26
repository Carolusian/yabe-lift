package code.lib

import net.liftweb.mapper._
import net.liftweb.http._
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import xml.{Group, Elem, NodeSeq}

trait MapperBinder {
  def bindMapper[T <: Mapper[T]](data: Mapper[T])(in: NodeSeq): NodeSeq = {
   /* in.foreach{ x =>
      println("-------")
      println(x)
      println("-------")
    }   */

    /*val in2 = in.flatMap {
      case e:Elem => {

      }
      case g:Group => {
        println("===")
        println(g)
        println("=====")
        g
      }
      case n => n
    }  */

    in.iterater.foreach{ x =>
      println("-------")
      println(x)
      println("-------")
    }

    ("*" #> <span>fdjsklfj</span>)(in)
  }
}

object MapperBinder extends MapperBinder