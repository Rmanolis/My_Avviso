package code.snippet
import scala.xml._
import net.liftweb._
import common._
import http._
import common._
import util._
import Helpers._
import code.model._
import code.lib._

class ChooseLanguage {
  def render = {
    "#chooseLanguage" #> {
      Language.findAll.map {
        lan =>
          "li" #> {
            <a href={ "/index?language=" + lan.name.is }>{ lan.name.is }</a>
          }
      }
    }

  }
}