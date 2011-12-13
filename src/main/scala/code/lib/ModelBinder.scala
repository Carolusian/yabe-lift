package code.lib

import net.liftweb.mapper._
import net.liftweb.common._
import net.liftweb.util._
import net.liftweb.http._
import js._
import Helpers._
import xml.transform.{RuleTransformer, RewriteRule}
import xml._

trait ModelBinder {
  val showSymbol = "@"
  val showSymbolReplacement = "&#64;"
  val showRegex = ("""\@[a-zA-Z0-9\_]+(\.[a-zA-Z0-9\_]+){0,1}""").r

  val inputSymbol = "mb:"
  val inputRegex = ("""mb\:[a-zA-Z0-9\_]+""").r

  //PersistentType
  type PT = {
    def fieldByName(fn:String): Box[FT]
  }
  
  //FieldType
  type FT = {
    def asHtml:scala.xml.Node
    def toForm:Box[NodeSeq]
  }
  
  //for ajax only
  //for form.ajax functions, this will makes ajaxForms function normally
  def bindModel(model: Any, 
      ajaxHandler:()=>JsCmd)(in:NodeSeq):NodeSeq = {
    bindModel(model)(in) ++ SHtml.hidden(ajaxHandler)
  }
  
  //for ajax only
  //for form.ajax functions, this will makes ajaxForms function normally
  def bindModel(model: Any, 
      ajaxHandler:()=>JsCmd, otherBinding:CssSel)(in:NodeSeq):NodeSeq = {
    otherBinding(bindModel(model)(in)) ++ SHtml.hidden(ajaxHandler)
  }
  
  def bindModel(model: Any, otherBinding:CssSel)(in:NodeSeq):NodeSeq = {
    otherBinding(bindModel(model)(in))
  }
  
  def bindModel(model: Any)(in: NodeSeq): NodeSeq = {
    val data = model.asInstanceOf[PT]
    
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
        val r = showRegex
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
          nodeStr = nodeStr.replace(m, fieldData )
        }

        //Because asHtml is already safe, so asHtml.toString is also safe. Just use Unparsed to avoid parsing & to &amp;
        Unparsed(nodeStr)
      }

      def getFieldForMatchFromData(matchStr:String):String = {
        //Remove @ sign
        val fieldName = matchStr.replace(showSymbol,"")

        val field = data.fieldByName(fieldName)
        field match {
          //replace @ to "&#64;", please note that you should also use "&#64;" instead of "@" in the templates
          case Full(f) => f.asInstanceOf[FT]
            .asHtml
            .toString
            .replace(showSymbol,showSymbolReplacement)
          case _ => invokeShowFunc(matchStr)
        }
      }

      //tried to invoke field.method function that return a Node, otherwise, return
      def invokeShowFunc(matchStr:String) = {
        val fieldNameMethod = matchStr.replace(showSymbol,"").split('.')
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

      def invokeFuncOfField(field:FT, method:String , matchStr:String):Node = {
        //If the method is validate method the has no argument and return a Node, invoke it
        val clazz = field.getClass
        val methods = clazz.getMethods()
        val validateMethods = methods.filter { m =>
          m.getName==method &&
            m.getParameterTypes().length==0 &&
            m.getReturnType == classOf[Node]
        }

        if(validateMethods.length > 0)
          validateMethods.head.invoke(field).asInstanceOf[Node]
        else
          Unparsed(matchStr) //Return original string if method is not found
      }
    }


    def tranformInput(in:NodeSeq):NodeSeq = {
      val r = inputRegex
      val classAttrs = (in \\ "@class").filter(n=>r.findFirstIn(n.text).getOrElse("")!="")
        .map(n=>r.findFirstIn(n.text).getOrElse(""))

      val validateAttrs = classAttrs.filter(isFieldValidate _)
      val cssSelectors = validateAttrs.map(bindInputField(_))

      cssSelectors.length match {
        case l if l > 0 => {
          val removeBindAttr = validateAttrs.map(removeInputBindAttr(_))
          removeBindAttr.reduceLeft(_ & _)(cssSelectors.reduceLeft(_ & _)(in))
        }
        case _ => in
      }
    }

    def isFieldValidate(classAttr:String):Boolean = {
      val fieldName = classAttr.replace(inputSymbol,"")
      val field = data.fieldByName(fieldName)
      field match {
        case Full(f) => {true}
        case _ => false
      }
    }

    def bindInputField(classAttr:String):CssSel = {
      val fieldName = classAttr.replace(inputSymbol,"")
      val field = data.fieldByName(fieldName).openTheBox
      //use dot to indicate that it is a class attribute
      ("."+classAttr) #> field.toForm
    }

    //remove mb:xxx from class attribute
    def removeInputBindAttr(classAttr:String):CssSel = {
      ("."+classAttr+" [class]") #> {
        val foundClassAttr = (in \\ "@class").filter(n=>n.text.contains(classAttr))
        foundClassAttr length match {
          case l if l>0 => foundClassAttr.head.text.replace(classAttr,"").trim
          case _ => ""
        }
      }
    }

    //transform show fields first, then transform input fields.
    // this will guarantee that '@fieldName' content in input fields would not be transformed by transformShow
    tranformInput(new RuleTransformer(tranformShow).transform(in))
  }
}

object ModelBinder extends ModelBinder