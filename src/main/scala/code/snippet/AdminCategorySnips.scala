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

class CRUDCategories {

  def tdForCategory(c: Category) = {

    <td> { c.name.is } </td> ++
      <td> { c.getLanguageName } </td> ++
      <td> { c.parent.obj.map(_.name.is).getOrElse("-") } </td> ++
      <td> {
        SHtml.ajaxButton(Text("Edit"), () => {
          RedirectTo(Site.editCategoryLoc.calcHref(c))

        })
      } </td> ++
      <td> {
        SHtml.ajaxButton(Text("Delete"), () => {
          c.delete
          RedirectTo(Site.crudCategories.fullUrl)

        })
      } </td>

  }
  def render(in: NodeSeq): NodeSeq = {
    <table id="adminListOfCategories">
      <tr>
        <th> Name </th>
        <th> Language </th>
        <th> Parent </th>
        <th> Edit </th>
        <th> Delete </th>
      </tr>
      {
        Category.findAll.filter(_.parent.isEmpty).map {
          c =>
            //TODO create  recursion to get childs
            <tr>
              {
                tdForCategory(c)
              }
            </tr> ++
              c.listOfChilds.map {
                ca =>
                  <tr>{
                    tdForCategory(ca)
                  }</tr>
              }

        }
      }
    </table>
  }
}

class AddCategory extends StatefulSnippet {
  def dispatch = { case _ => render }

  val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
  val categories = List(("-1", "None")) ++ Category.findAll.filter(_.parent.obj.isEmpty).map(c => (c.id.is.toString, c.name.is))
  val hLang = languages.headOption.map(_._1)
  val hCat = categories.headOption.map(_._1)
  var language = ""
  var category = ""
  var name = ""

  def render(in: NodeSeq): NodeSeq = {

    ("#name" #> SHtml.text("", name = _) &
      "#languages" #> SHtml.select(languages, hLang, language = _, "id" -> "languages") &
      "#categories" #> SHtml.select(categories, hCat, category = _, "id" -> "categories") &
      "#addButton" #> SHtml.button(Text("Submit"), () => {
        var catObj: Box[Category] = Empty

        for {
          l <- Language.find(language)

        } yield {
          catObj = Category.add(l, name)
        }

        catObj.map {
          cat =>
            Category.find(cat).map {
              parent =>

                cat.addParent(parent)

            }
            S.redirectTo(Site.crudCategories.fullUrl)
        }

      }) &
      "#cancelButton" #> SHtml.ajaxButton(Text("Cancel"), () => {
        RedirectTo(Site.crudCategories.fullUrl)

      })).apply(in)

  }

}

class EditCategory extends StatefulSnippet {
  def dispatch = { case _ => render }
  var parent = ""
  val languages = Language.findAll.map(l => (l.id.is.toString, l.name.is))
  var language = ""

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      category <- Site.editCategoryLoc.currentValue
    } yield {
      var name = category.name.is
      val categories = Category.findAll
        .filter(_.id.is != category.id.is)
        .filter(_.parent.obj.isEmpty)
        .map(ca => (ca.id.is.toString, ca.name.is)) ++ List(("-1", "None"))
      parent = category.parent.obj.map(_.id.is.toString).getOrElse("-1")
      language = category.language.is.toString
      out = ("#name" #> SHtml.text(name, name = _) &
        "#languages" #> SHtml.select(languages, Full(language), language = _, "id" -> "languages") &
        "#categories" #> SHtml.select(categories, Full(parent), parent = _, "id" -> "categories") &
        "#addButton" #> SHtml.button(Text("Submit"), () => {

          var catObj: Box[Category] = Empty
          Language.find(language).map {
            l =>
              catObj = category.edit(l, name)
          }
          if (!catObj.isEmpty) {
            Category.find(parent).map {
              par =>
                category.addParent(par)
            }.getOrElse {
              category.removeParent
            }

            S.redirectTo(Site.crudCategories.fullUrl)
          }

        }) &
        "#cancelButton" #> SHtml.ajaxButton(Text("Cancel"), () => {
          RedirectTo(Site.crudCategories.fullUrl)

        })).apply(in)
    }

    out
  }
}

