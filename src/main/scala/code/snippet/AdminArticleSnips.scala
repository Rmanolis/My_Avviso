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
import net.liftweb.http.js.jquery.JqJE.Jq
import net.liftweb.http.js.jquery.JqJE.JqGetAttr
import scala.collection.mutable.HashMap
import net.liftweb.mapper.{ StartAt, MaxRows, OrderBy, By, Descending }

class CRUDArticleSnippet extends PaginatorSnippet[Article] {
  val selectCategory = List(("-1", "All")) ++ Category.findAll.map(c => (c.id.is.toString, c.name.is))
  val selectLanguage = List(("-1", "All")) ++ Language.findAll.map(l => (l.id.is.toString, l.name.is))
  var params = "?offset=" + (curPage * itemsPerPage).toString + "&"
  var category = S.param("catArticles").getOrElse("-1")
  var language = S.param("lanArticles").getOrElse("-1")
  def showDrafts = {
    var is =  S.param("showDrafts").getOrElse("0")
    if(is== "0") false else true 		
  }
  override def count = Article.count
  override def itemsPerPage = 8
  override def page = {
    var articles:List[Article] = List()
    if (category == "-1" && language == "-1") {
      articles = Article.findAll(OrderBy(Article.dateTime, Descending), StartAt(curPage * itemsPerPage), MaxRows(itemsPerPage))
    } else if (language == "-1" && category != "-1") {
      articles = Article.findAll(
        By(Article.category, Category.find(category)),
        OrderBy(Article.dateTime, Descending),
        StartAt(curPage * itemsPerPage),
        MaxRows(itemsPerPage))
    } else  if (language != "-1" && category == "-1"){
      val categories = Category.findAll(By(Category.language, Language.find(language)))

     articles = categories.map {
        c =>
          Article.findAll(
            By(Article.category, c),
            OrderBy(Article.dateTime, Descending),
            StartAt(curPage * itemsPerPage),
            MaxRows(itemsPerPage))
      }.flatten

    } else {
      articles = Article.findAll(
            By(Article.category, Category.find(category)),
            OrderBy(Article.dateTime, Descending),
            StartAt(curPage * itemsPerPage),
            MaxRows(itemsPerPage))
    }
    articles.filter(a=> a.isDraft == showDrafts)
    
  }
  override def pageUrl(offset: Long): String = appendParams(super.pageUrl(offset), List("language" -> "-1", "category" -> "-1"))
  def cleanArticles {
    Article.findAll.filter(_.title.is.isEmpty).map { a =>
      a.delete
    }
  }

  def renderPage(in: NodeSeq): NodeSeq = {

    cleanArticles

    (
      "#checkDrafts" #> SHtml.checkbox(showDrafts, b => {
        if (b) {
          params += "showDrafts=1&"
        }
      }) &
      "#selectCategory" #> SHtml.select(selectCategory, Full(category), s => {
        params += "catArticles=" + s + "&"
      }) &
      "#selectLanguage" #> SHtml.select(selectLanguage, Full(language), s => {
        params += "lanArticles=" + s + "&"
      }) &
      "#submit" #> SHtml.button(Text("Submit"), () => {
        S.redirectTo(Site.crudArticles.fullUrl + params)
      }) &
      "#adminListOfArticles" #> <table id="adminListOfArticles"> {
        <tr>
          <th>Choose</th>
          <th>Title</th>
          <th>Category</th>
          <th>Draft</th>
          <th>Add Files</th>
          <th>Edit</th>
          <th>Delete</th>
        </tr> ++
          page.map {
            a =>
              <tr> {
                <td> {
                  SHtml.checkbox(false, b => {
                    if (b) {
                      a.delete
                    }
                  })
                } </td> ++
                  <td> { a.title.is } </td> ++
                  <td> { a.getCategoryName } </td> ++
                  <td> { a.isDraft.is } </td> ++
                  <td> {
                    button(Text("Upload Files"), () => {
                      S.redirectTo(Site.addArticleFileLoc.calcHref(a))

                    })
                  } </td> ++
                  <td> {
                    button(Text("Edit"), () => {
                      S.redirectTo(Site.editArticleLoc.calcHref(a))

                    })
                  } </td> ++
                  <td> {
                    button(Text("Delete"), () => {
                      a.delete
                      S.redirectTo(Site.crudArticles.fullUrl)
                    })
                  } </td>
              }</tr>
          }
      }</table> &
      "#delete" #> SHtml.button(Text("Delete"), () => {
        S.redirectTo(Site.crudArticles.fullUrl)
      })).apply(in)
  }
}

class AddArticle extends StatefulSnippet {
  val categoriesObj = Category.findAll
  var categories = categoriesObj.map(c => (c.id.is.toString, c.name.is))
  var category = categories.headOption.map(_._1).getOrElse("")
  var title = ""
  var article = ""
  var isDraft = false
  var articleDB: Box[Article] = None
  articleDB = Article.quickAdd(title, article, isDraft)

  object GeolocateJson extends JsonHandler {
    def apply(in: Any): JsCmd = {
      def rec(): JsCmd = {
        SetHtml("json_result", in match {
          case JsonCmd("show", _, "list", _) => {
            articleDB.map {
              a =>
                <ul id="json_result"> {
                  a.getLocations.map {
                    loc =>
                      <li>{
                        <span>{ loc.explanation.is } </span> ++
                          SHtml.ajaxButton(Text("delete"), () => {
                            loc.delete_!
                            JsRaw(GeolocateJson.call("show", "list")).cmd
                          })
                      }</li>
                  }
                } </ul>
            }.getOrElse(<ul id="json_result"></ul>)
          }
          case x => <b>Problem... didn't handle JSON message { x }</b>
        })
      }
      rec
    }
  }

  def addLatLng(in: NodeSeq): NodeSeq = {
    var lat = ""
    var lng = ""
    var explanation = ""

    ("#json_script" #> Script(GeolocateJson.jsCmd) &
      "#explanation" #> SHtml.ajaxTextarea(explanation, explanation = _, "id" -> "explanation") &
      "#yes" #> SHtml.ajaxButton(Text("Submit"), () =>
        {

          ajaxCall(JsRaw("""document.getElementById('lat').innerHTML"""), s => {
            lat = s
            Noop
          }) &
            ajaxCall(JsRaw("""document.getElementById('lng').innerHTML"""), s => {
              lng = s

              for {
                a <- articleDB
                latitude <- asDouble(lat)
                longtitude <- asDouble(lng)
              } yield {
                GeolocateArticle.add(a, explanation, latitude, longtitude)
              }
              SetValById("lat", "") &
                SetValById("lng", "") &
                SetValById("explanation", "") &
                JsRaw(""" jQuery('#dialog-form' ).dialog( 'close' )""").cmd &
                JsRaw(GeolocateJson.call("show", "list")).cmd

            })

        }) &
      "#no" #> SHtml.ajaxButton(Text("Cancel"), () => {
        JE.JsRaw(""" jQuery('#dialog-form' ).dialog( 'close' )""").cmd
      })).apply(in)
  }

  val dispatch: DispatchIt = {
    case "addArticle" => addArticle _
    case "addLatLng" => addLatLng _
  }

  def addArticle(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty

    out = ("#title" #> SHtml.text(title, title = _) &
      "#categories" #> SHtml.select(categories, Full(category), category = _, "id" -> "categories") &
      "#article" #> SHtml.textarea(article, article = _) &
      "#isDraft" #> SHtml.checkbox(isDraft, isDraft = _) &
      "#addButton" #> SHtml.button(Text("Submit"), () => {

        for {
          cat <- Category.find(category)
          aObj <- articleDB
          a <- aObj.edit(cat, title, article, isDraft)
        } yield {

          articleDB = None
          S.redirectTo(Site.crudArticles.url)
        }

      }) &
      "#cancelButton" #> SHtml.button(Text("Cancel"), () => {
        for (a <- articleDB) yield {
          a.delete
        }
        articleDB = None
        S.redirectTo(Site.crudArticles.url)

      })).apply(in)

    out
  }

}

class EditArticle extends StatefulSnippet {
  var categories = Category.findAll.map(c => (c.id.is.toString, c.name.is))
  var category = categories.headOption.map(_._1).getOrElse("")

  var title = ""
  var article = ""
  var isDraft = true
  var articleDB: Box[Article] = Site.editArticleLoc.currentValue
  object GeolocateJson extends JsonHandler {
    def apply(in: Any): JsCmd = {
      def rec(): JsCmd = {
        SetHtml("json_result", in match {
          case JsonCmd("show", _, "list", _) => {
            articleDB.map {
              a =>
                <ul id="json_result"> {
                  a.getLocations.map {
                    loc =>
                      <li>{
                        <span>{ loc.explanation.is } </span> ++
                          SHtml.ajaxButton(Text("delete"), () => {
                            loc.delete_!
                            JsRaw(GeolocateJson.call("show", "list")).cmd
                          })
                      }</li>
                  }
                } </ul>
            }.getOrElse(<ul id="json_result"></ul>)
          }
          case x => <b>Problem... didn't handle JSON message { x }</b>
        })
      }
      rec
    }
  }

  def addLatLng(in: NodeSeq): NodeSeq = {
    var lat = ""
    var lng = ""
    var explanation = ""

    ("#json_script" #> Script(GeolocateJson.jsCmd) &
      "#explanation" #> SHtml.ajaxTextarea(explanation, explanation = _, "id" -> "explanation") &
      "#yes" #> SHtml.ajaxButton(Text("Submit"), () =>
        {

          ajaxCall(JsRaw("""document.getElementById('lat').innerHTML"""), s => {
            lat = s
            Noop
          }) &
            ajaxCall(JsRaw("""document.getElementById('lng').innerHTML"""), s => {
              lng = s

              for {
                a <- articleDB
                latitude <- asDouble(lat)
                longtitude <- asDouble(lng)
              } yield {
                GeolocateArticle.add(a, explanation, latitude, longtitude)
              }
              SetValById("lat", "") &
                SetValById("lng", "") &
                SetValById("explanation", "") &
                JsRaw(""" jQuery('#dialog-form' ).dialog( 'close' )""").cmd &
                JsRaw(GeolocateJson.call("show", "list")).cmd

            })
        }) &
      "#no" #> SHtml.ajaxButton(Text("Cancel"), () => {
        JE.JsRaw(""" jQuery('#dialog-form' ).dialog( 'close' )""").cmd
      })).apply(in)
  }

  val dispatch: DispatchIt = {

    case "editArticle" => editArticle _
    case "showLocations" => showLocations _
    case "addLatLng" => addLatLng _
  }

  def showLocations(in: NodeSeq): NodeSeq = {
    articleDB.map {
      a =>
        <ul id="json_result"> {
          a.getLocations.map {
            loc =>
              <li>{
                <span>{ loc.explanation.is } </span> ++
                  SHtml.ajaxButton(Text("delete"), () => {
                    loc.delete_!
                    GeolocateJson.apply(in)
                  })
              }</li>
          }
        } </ul>
    }.getOrElse(<ul id="json_result"></ul>)

  }

  def editArticle(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty

    for {
      aObj <- Site.editArticleLoc.currentValue

    } yield {
      val categories = Category.findAll.map(c => (c.id.is.toString, c.name.is))
      var title = aObj.title.is
      var article = aObj.article.is
      var category = aObj.category.is.toString
      var isDraft = aObj.isDraft.is
      out = ("#title" #> SHtml.text(title, title = _) &
        "#categories" #> SHtml.select(categories, Full(category), category = _, "id" -> "categories") &
        "#article" #> SHtml.textarea(article, article = _) &
        "#isDraft" #> SHtml.checkbox(isDraft, isDraft = _) &
        "#addButton" #> SHtml.button(Text("Submit"), () => {

          for {
            cat <- Category.find(category)
            a <- aObj.edit(cat, title, article, isDraft)
          } yield {

            articleDB = None
            S.redirectTo(Site.crudArticles.url)
          }

        }) &
        "#cancelButton" #> SHtml.ajaxButton(Text("Cancel"), () => {
          articleDB = None
          RedirectTo(Site.crudArticles.url)

        })).apply(in)

    }
    out
  }
}






