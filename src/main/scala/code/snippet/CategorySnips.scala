package code.snippet
import scala.xml._
import net.liftweb._
import common._
import http.{ DispatchSnippet, S, SHtml, StatefulSnippet }
import http.js.JsCmd
import http.js.JsCmds._
import util._
import Helpers._
import code.model._
import code.lib._
class ListOfCaregories {
  def listOfChilds(category: Category) = {
    val cats = category.listOfChilds
    if (cats.isEmpty) {
      NodeSeq.Empty
    } else {
      <ul>{
        cats.map {
          cat =>
            <li> {
              Site.chooseCategoryLoc.currentValue.map {
                sc =>
                  if (cat.name.is == sc.name.is) {
                    <span> { cat.name.is } </span>
                  } else {
                    <a href={ ("/category/" + cat.name.is) }>{ cat.name.is }</a>
                  }
              }.getOrElse {

                <a href={ ("/category/" + cat.name.is) }>{ cat.name.is }</a>

              }

            }</li>
        }
      }</ul>
    }
  }
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    MyHelpers.getLanguage.map {
      lang =>
        out = <ul> {
          Category.findByLanguage(lang).filter(_.parent.isEmpty).map {
            c =>
              <li> {
                Site.chooseCategoryLoc.currentValue.map {
                  sc =>
                    if (c.name.is == sc.name.is) {
                      <span> { c.name.is } </span>
                    } else {
                      
                      <a href={ ("/category/" + c.name.is) }>{ c.name.is }</a> ++
                      listOfChilds(c)
                    }
                }.getOrElse {

                  <a href={ ("/category/" + c.name.is) }>{ c.name.is }</a> ++
                  listOfChilds(c)
                }

              }</li>
          }
        }</ul>
    }
    out
  }
}

