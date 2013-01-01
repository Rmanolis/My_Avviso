package code.snippet
import net.liftweb._
import common._
import http.{ DispatchSnippet, S, SHtml, StatefulSnippet }
import http.js.JsCmd
import http.js.JsCmds._
import util._
import Helpers._
import scala.xml._

import code.lib._
import code.model._

class AddLanguage extends StatefulSnippet {
  def dispatch = { case _ => render }
  var title = ""
  var check = false
  def render(in: NodeSeq): NodeSeq = {
    import SHtml._

    var out = NodeSeq.Empty
    User.currentUser.map {
      x =>
        out = (
          "#name" #> text(title, title = _) &
          "#checkDefault" #> checkbox(check, b => {
            check = b
          }) &
          "#addButton" #> button(Text("Submit"), () => {

            if (!Language.add(title, check).isEmpty) {
              S.redirectTo(Site.crudLanguages.fullUrl)
            }

          }) &
          "#cancelButton" #> button(Text("Cancel"), () => {
            S.redirectTo(Site.crudLanguages.fullUrl)

          })).apply(in)
    }
    out
  }
}

class EditLanguage extends StatefulSnippet {
  def dispatch = { case _ => render }

  var title = ""
  var check = false
  def render(in: NodeSeq): NodeSeq = {
    import SHtml._
    var out = NodeSeq.Empty
    for {
      user <- User.currentUser
      language <- Site.editLanguageLoc.currentValue
    } yield {
      title = language.name.is
      check = language.isDefault.is
      out = (
        "#name" #> text(title, title = _) &
        "#checkDefault" #> checkbox(check, b => {
          check = b
        }) &
        "#addButton" #> button(Text("Submit"), () => {

          if (!language.edit(title, check).isEmpty) {
            S.redirectTo(Site.crudLanguages.fullUrl)
          }

        }) &
        "#cancelButton" #> button(Text("Cancel"), () => {
          S.redirectTo(Site.crudLanguages.fullUrl)

        })).apply(in)
    }

    out
  }

}

object CRUDLanguages {
  def render(in: NodeSeq): NodeSeq = {
    import SHtml._

    <table id="listLanguages"> {
      <tr>
        <th> Name </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr> ++
        Language.findAll.map {
          language =>
            <tr> {
              <td> { language.name.is } </td> ++
                <td> {
                  ajaxButton(Text("Edit"), () => {
                    RedirectTo(Site.editLanguageLoc.calcHref(language))

                  })
                }</td> ++
                <td> {
                  ajaxButton(Text("Delete"), () => {
                    language.delete
                    RedirectTo(Site.crudLanguages.fullUrl)
                  })
                }</td>
            }</tr>
        }
    }</table>

  }
}