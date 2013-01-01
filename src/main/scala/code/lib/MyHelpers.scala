package code.lib
import scala.xml._
import net.liftweb._
import common._
import http._
import json._
import util.CssSel
import util.Helpers._
import org.jsoup.Jsoup
import net.liftweb.http.SessionVar
import code.model._

object languageSession extends SessionVar[Box[Language]](Empty)

object MyHelpers {
  def htmlToString(html: String) = {
    Jsoup.parse(html).text()
  }

  def stringToElem(s: String) = XML.loadString("<lift:children>" + s + "</lift:children>")
  
  def getLanguage : Box[Language] = {
    S.param("language").map {
      lang =>
        val lan = Language.findByName(lang)
        languageSession.set(lan )
       
    }.getOrElse {
      var langOpt: Box[Language] = None
      if (languageSession.get.isEmpty) {
        langOpt = Language.findDefault
      } else {
        langOpt = languageSession.get
      }
      langOpt
    }

  }
  
  
}

object MyErrors{
  def checkLimitError(str:String, id:String,limit:Int)={
     if(str.size > limit){
       S.error(id,"Too long. The limit is "+limit.toString+" characters" )
       true
     }else{
       false
     }    
  }
  
  
}