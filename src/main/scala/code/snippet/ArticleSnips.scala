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

class ListOfArticlesByCategory {
  def render(in: NodeSeq) = {
    var out = NodeSeq.Empty

    for {

      cat <- Site.chooseCategoryLoc.currentValue
    } yield {
      out = ("#listOfArticles" #> {
        Article.findByCategory(cat).filter(_.isDraft.is == false).map {
          a =>
            "li" #> {

              "#title" #> <a href={ Site.chooseArticleLoc.calcHref(a) }> { a.title.is } </a> &
                "#summary" #> a.getSummary.take(200)
            }
        }
      }).apply(in)
    }
    out
  }
}

class ShowArticle {
  def render(in: NodeSeq) = {
    var out = NodeSeq.Empty
    for {
      article <- Site.chooseArticleLoc.currentValue
    } yield {
      out = (
        "#title" #> article.title.is &
        "#article" #> Unparsed(article.article.is)).apply(in)
    }
    out
  }
}

class ListOfArticlesByLanguage {
  def render(in: NodeSeq): NodeSeq = {
    ("#listOfArticles" #> {
      Article.findAll.filter(_.isDraft.is == false).map {
        a =>
          "li" #> {
            "#title" #> a.title.is &
              "#article" #> a.article.is
          }
      }
    }).apply(in)
  }
}

class LatestArticlesByCategory {
  import MyHelpers._
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for (lan <- getLanguage) yield {
      out = ("#listOfArticles" #> {
        Category.findByLanguage(lan).map {
          c =>
            "li" #> {
              "#category" #> c.name.is &
                "ul" #> {
                  Article.findByCategory(c).filter(_.isDraft.is == false).take(3).map {
                    a =>
                      "li" #> {
                        "#image" #> a.getSmallPictureHtml &
                          "#title" #> <a href={ Site.chooseArticleLoc.calcHref(a) }> { a.title.is } </a> &
                          "#summary" #> a.getSummary.take(200)
                      }
                  }
                }
            }
        }
      }).apply(in)
    }
    out
  }
}

class ShowFilesByArticle {
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      article <- Site.chooseArticleLoc.currentValue
    } yield {
      out = (
        "#listOfFiles" #> FileArticle.findByArticle(article).map {
          fa =>
            "li" #> {
              "img" #> fa.file.obj.map(_.getImgHtml(200,200)).getOrElse(NodeSeq.Empty) &
              "#name *" #> fa.file.obj.map(_.name.is).getOrElse("")
            }
        }).apply(in)
    }
    out
  }
}

class ShowTheMainImageOfTheArticle{
  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      article <- Site.chooseArticleLoc.currentValue
    } yield {
      out = FileArticle.findByArticle(article).filter{
        fa => 
          fa.file.map{
            f => 
              FileType.mapped(f.fileType.is.id) == "Photo"
          }.getOrElse{
            false
          }
      }.sortBy(_.priority.is)
      .headOption
      .map(_.file.map(_.getImgHtml(400,400))
          .getOrElse(NodeSeq.Empty))
          .getOrElse(NodeSeq.Empty)
    }
    out
  }
}


