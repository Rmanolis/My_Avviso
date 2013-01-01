package code.snippet

import code.lib.MyErrors
import scala.xml._
import net.liftweb._
import common._
import http.{ DispatchSnippet, S, SHtml, StatefulSnippet }
import http.js.JsCmd
import http.js.JsCmds._
import util._
import Helpers._
import code.model._
import code.lib.Site

class CRUDLocalizedLabels {
  def render = {
    <table id="listOfLocalesByLabel"> {
      <tr>
        <th> Label </th>
        <th> Text </th>
        <th> Language </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr> ++
        Localization.findAll.sortBy(_.label.is).map {
          ll =>
            <tr> {
              <td> { ll.label.is }</td> ++
                <td> { ll.text.is }</td> ++
                <td> { ll.getLanguageName } </td> ++
                <td>{
                  SHtml.ajaxButton(Text("Edit"), () => {
                    RedirectTo(Site.editLocalizedLabelLoc.calcHref(ll))

                  })
                }</td> ++
                <td> {
                  SHtml.ajaxButton(Text("Delete"), () => {
                    ll.delete_!
                    RedirectTo(Site.crudLocalizedLabels.fullUrl)

                  })
                }</td>
            }</tr>
        }
    } </table>
  }
}

class AddLocalizedLabel extends StatefulSnippet {
  val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
  var language = ""
  var label = ""
  var text = ""
  def dispatch = { case _ => render }

  def render(in: NodeSeq): NodeSeq = {

    ("#label" #> SHtml.text("", label = _) &
      "#text" #> SHtml.textarea("", text = _) &
      "#languages" #> SHtml.select(languages, Empty, language = _, "id" -> "languages") &
      "#addButton" #> SHtml.button(Text("Submit"), () => {
       
          Language.find(language).map {
            l =>
              val a = Localization.add(l, label, text) 
              if(!a.isEmpty){
                 S.redirectTo(Site.crudLocalizedLabels.fullUrl)
              }
          }
          
        
      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudLocalizedLabels.fullUrl)

      })).apply(in)

  }

}

class EditLocalizedLabel extends StatefulSnippet {
  val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
  var language = ""
  var label = ""
  var text = ""
  def dispatch = { case _ => render }
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      langObj <- Site.editLocalizedLabelLoc.currentValue
    } yield {
      label = langObj.label.is
      text = langObj.text.is
      language = langObj.language.is.toString
      out = ("#label" #> SHtml.text(label, label = _) &
        "#text" #> SHtml.textarea(text, text = _) &
        "#languages" #> SHtml.select(languages, Full(language), language = _, "id" -> "languages") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {

          Language.find(language).map {
            l =>
              if (!langObj.edit(label, text, l).isEmpty) {
                S.redirectTo(Site.crudLocalizedLabels.fullUrl)
              }
          }

        }) &
        "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudLocalizedLabels.fullUrl)

        })).apply(in)
    }

    out
  }
}

