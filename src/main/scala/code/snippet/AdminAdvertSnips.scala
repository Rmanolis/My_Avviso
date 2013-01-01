package code.snippet
import scala.xml._
import net.liftweb._
import common._
import http._
import SHtml._
import js._
import JE._
import JsCmds._
import js.jquery._
import JqJsCmds._
import common._
import util._
import Helpers._
import code.model._
import code.lib._

class CRUDAdverts {
  def render = {
    <table id="adminListOfAdverts"> {
      <tr>
        <th>Company</th>
        <th>Product Name</th>
        <th>Email</th>
        <th>Language</th>
        <th>Edit</th>
        <th>Delete</th>
      </tr> ++
        Advert.findAll.map {
          a =>
            <tr> {
              <td> { a.company.is } </td> ++
                <td> { a.productName } </td> ++
                <td> { a.email.is } </td> ++
                <td> { a.getLanguageName } </td> ++
                <td> {
                  SHtml.ajaxButton(Text("Edit"), () => {
                    RedirectTo(Site.editAdvertLoc.calcHref(a))

                  })
                } </td> ++
                <td> {
                  SHtml.ajaxButton(Text("Delete"), () => {
                    a.delete_!
                    RedirectTo(Site.crudAdverts.fullUrl)
                  })
                } </td>
            }</tr>
        }
    }</table>
  }
}

class AddAdvert extends StatefulSnippet {

  val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
  var language = ""
  var company = ""
  var product = ""
  var telephone = ""
  var email = ""
  var introduction = ""
  var html = ""

  def dispatch = { case _ => render }

  def render = {

    "#company" #> SHtml.text(company, company = _) &
      "#languages" #> SHtml.select(languages, None, language = _) &
      "#product" #> SHtml.text(product, product = _) &
      "#telephone" #> SHtml.text(telephone, telephone = _) &
      "#email" #> SHtml.text(email, email = _) &
      "#introduction" #> SHtml.textarea(introduction, introduction = _) &
      "#html" #> SHtml.textarea(html, html = _) &
      "type=submit" #> SHtml.onSubmitUnit(() => {

        Language.find(language).map {
          lang =>

            if (!Advert.add(lang, company, product, telephone, email, introduction, html).isEmpty) {
              S.redirectTo(Site.crudAdverts.fullUrl)
            }

        }.getOrElse {
          S.redirectTo(Site.crudAdverts.fullUrl)
        }
      }) &
      "#cancel" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudAdverts.fullUrl)
      })

  }
}

class EditAdvert extends StatefulSnippet {
  val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
  var language = ""
  var company = ""
  var product = ""
  var telephone = ""
  var email = ""
  var introduction = ""
  var html = ""

  def dispatch = { case _ => render }

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      advert <- Site.editAdvertLoc.currentValue
    } yield {
      language = advert.language.is.toString
      company = advert.company.is
      product = advert.productName.is
      telephone = advert.telephone.is
      email = advert.email.is
      introduction = advert.introduction.is
      html = advert.html.is

      out = ("#company" #> SHtml.text(company, company = _) &
        "#languages" #> SHtml.select(languages, Full(language), language = _) &
        "#product" #> SHtml.text(product, product = _) &
        "#telephone" #> SHtml.text(telephone, telephone = _) &
        "#email" #> SHtml.text(email, email = _) &
        "#introduction" #> SHtml.textarea(introduction, introduction = _) &
        "#html" #> SHtml.textarea(html, html = _) &
        "type=submit" #> SHtml.onSubmitUnit(() => {

          Language.find(language).map {
            lang =>

              if (!advert.edit(lang, company, product, telephone, email, introduction, html).isEmpty) {
                S.redirectTo(Site.crudAdverts.fullUrl)
              }

          }
        }) &
        "#cancel" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudAdverts.fullUrl)
        })).apply(in)
    }
    out

  }

}