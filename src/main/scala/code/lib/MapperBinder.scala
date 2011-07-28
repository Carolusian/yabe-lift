package code.lib

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.util._
import Helpers._
import xml.transform.{RuleTransformer, RewriteRule}
import xml._

trait MapperBinder {
  def bindMapper[T<:Mapper[T]](data: Mapper[T])(in:NodeSeq):NodeSeq = {
    bindMapper(data, "SomeThingThatShouldBeImpossibleToBeUsed" #> "")(in)
  }
  def bindMapper[T<:Mapper[T]](data: Mapper[T], otherBinding:CssSel)(in: NodeSeq): NodeSeq = {
    val tranformShow = new RewriteRule {
      override def transform(n:Node):NodeSeq = n match {
        //check if text has @fieldName
        case e:Elem => {
          new Elem(e.prefix,
            e.label,
            e.attributes,
            e.scope,
            replace(transform(e.child)): _*)
        }
        case n => n
      }

      def replace(ns:NodeSeq):NodeSeq = {
        val r = """\@[a-zA-Z0-9\_]+(\.[a-zA-Z0-9\_]+){0,1}""".r
        ns flatMap { n =>
          val matches = r.findAllIn(n.toString).toList
          if(matches.length > 0) {
            replaceMatchesWithData(n, matches)
          } else {
            n
          }
        }
      }

      def replaceMatchesWithData(n:Node, matches:List[String]):Node = {
        var nodeStr = n.toString
        matches.foreach { m =>
          val fieldData = getFieldForMatchFromData(m)
          nodeStr = nodeStr.replaceAllLiterally(m, fieldData )
        }

        //Because asHtml is already safe, so asHtml.toString is also safe. Just use Unparsed to avoid parsing & to &amp;
        Unparsed(nodeStr)
      }

      def getFieldForMatchFromData(matchStr:String):String = {
        //Remove @ sign
        val fieldName = matchStr.replaceAllLiterally("@","")

        val field =  data.fieldByName(fieldName)
        field match {
          //replace @ to "&#64;", please note that you should also use "&#64;" instead of "@" in the templates
          case Full(f) => f.asInstanceOf[MappedField[_,T]]
            .asHtml
            .toString
            .replaceAllLiterally("@","&#64;")
          case _ => invokeShowFunc(matchStr)
        }
      }

      //tried to invoke field.method function that return a Node, otherwise, return
      def invokeShowFunc(matchStr:String) = {
        val fieldNameMethod = matchStr.replaceAllLiterally("@","").split('.')
        fieldNameMethod.length match {
          case l if (l==2) => {
            val field = data.fieldByName(fieldNameMethod(0))
            field match {
              case Full(f) => {
                invokeFuncOfField(f, fieldNameMethod(1) , matchStr).toString
              }
              case _ => matchStr
            }
          }
          case _ => matchStr
        }
      }

      def invokeFuncOfField[A](field:MappedField[A,T], method:String , matchStr:String):Node = {
        //If the method is validate method the has no argument and return a Node, invoke it
        val clazz = field.getClass
        val methods = clazz.getMethods()
        val validateMethods = methods.filter { m =>
          m.getName==method &&
            m.getParameterTypes().length==0 &&
            m.getReturnType == classOf[Node]
        }

        if(validateMethods.length  > 0)
          validateMethods.head.invoke(field).asInstanceOf[Node]
        else
          Unparsed(matchStr)     //Return original string if method is not found
      }
    }


    def tranformInput(in:NodeSeq):NodeSeq =  {
      val classAttrs = (in \\ "@class").filter(n=>n.text.contains("mb:")).map(n=>n.text)
      val validateAttrs = classAttrs.filter(isFieldValidate _)
      val cssSelectors = validateAttrs.map(bindInputField(_))

      if(cssSelectors.length > 0)
        cssSelectors.reduceLeft(_ & _)(in)
      else
        in
    }

    def isFieldValidate(classAttr:String):Boolean = {
      val fieldName = classAttr.replaceAllLiterally("mb:","")
      val field = data.fieldByName(fieldName)
      field match {
        case Full(f) => {true}
        case _ => false
      }
    }

    def bindInputField(classAttr:String):CssSel = {
      val fieldName = classAttr.replaceAllLiterally("mb:","")
      val field = data.fieldByName(fieldName).openTheBox
      //use dot to indicate that it is a class attribute
      ("."+classAttr) #> field.toForm
    }

    otherBinding(tranformInput(new RuleTransformer(tranformShow).transform(in)))
  }
}

object MapperBinder extends MapperBinder