package code.snippet
import scala.xml._
import net.liftweb._
import common._
import http._
import mapper._
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

class CRUDFiles extends PaginatorSnippet[AvvisoFile] {
  override def count = AvvisoFile.count
  override def itemsPerPage = 10
  override def page = {
    var list: List[QueryParam[AvvisoFile]] = List()
    list +:= OrderBy(AvvisoFile.id, Descending)
    list +:= StartAt(curPage * itemsPerPage)
    list +:= MaxRows(itemsPerPage)
    AvvisoFile.findAll(list: _*)
  }

  def renderPage(in:NodeSeq):NodeSeq = {
    <table id="listOfFiles"> {
      <tr>
        <th> ID </th>
        <th> Name </th>
        <th> Type </th>
        <th> Delete </th>
      </tr> ++
        page.map {
          file =>
            <tr> {
              <td> { file.id.is } </td> ++
                <td> { file.name.is }</td> ++
                <td> { FileType.mapped(file.fileType.is.id) }</td> ++
                <td> {
                  SHtml.button(Text("Delete"), () => {
                    file.delete
                    S.redirectTo(Site.crudFiles.fullUrl)

                  })
                }</td>
            }</tr>
        }
    } </table>
  }
}

private object theUpload extends RequestVar[Box[FileParamHolder]](Empty)

class AddFile extends StatefulSnippet {

  val fileTypes = FileType.mapped.map(m => (m._1.toString, m._2)).toSeq
  var ft = fileTypes.head._1
  var name = ""

  def dispatch = { case _ => render }

  def render = {

    "#name" #> SHtml.text("", name = _) &
      "#upload" #> SHtml.fileUpload(ul => theUpload(Full(ul))) &
      "#chooseFileTypes" #> SHtml.select(fileTypes, Full("1"), s => {
        ft = s
      }) &
      "type=submit" #> SHtml.onSubmitUnit(() => {
        if (!MyErrors.checkLimitError(name, "name", 220)) {

          for {
            file <- theUpload
            ftInt <- asInt(ft)
            fileObj <- AvvisoFile.add(AvvisoOption.getGlobalFilePath, name, FileType.apply(ftInt))
          } yield {
            val ftype = FileType.mapped(ftInt)

            if (ftype == "Photo") {
              val rfBI = ImageHelpers.resize(file.fileStream, 140, 130)
              ImageHelpers.saveBI(AvvisoOption.getGlobalFilePath + fileObj.id.get + "_140x130", rfBI)
              FileHelpers.save(file.fileStream, AvvisoOption.getGlobalFilePath + fileObj.id.get)
            } else {
              FileHelpers.save(file.fileStream, AvvisoOption.getGlobalFilePath + fileObj.id.get)
            }

            theUpload(Empty)

            S.redirectTo(Site.crudFiles.fullUrl)
          }

        }
      }) &
      "#cancel" #> SHtml.button(Text("Cancel"), () => {
        S.redirectTo(Site.crudArticles.fullUrl)
      })
  }
}

class AddFileToArticle extends StatefulSnippet {

  def dispatch = {
    case "tableOfArticleFiles" => tableOfArticleFiles
    case "addFile" => addFile
  }

  def tableOfArticleFiles(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    var submit = NodeSeq.Empty
    for {
      article <- Site.addArticleFileLoc.currentValue
    } yield {
      val files = article.getFiles
      out = <table id="listOfArticleFiles"> {
        <tr>
          <th> Name </th>
          <th> Type </th>
    	  <th> Up </th>	  
          <th> Prioriy </th>
          <th> Down </th>
          <th> Delete </th>
        </tr> ++
          files.map {
            case (fa, fileObj) =>
              fileObj.map {
                file =>
                  <tr> {
                    <td> { file.name.is }</td> ++
                      <td> { FileType.mapped(file.fileType.is.id) }</td> ++
                      <td> { SHtml.button(Text("Up") , ()=> {
                        fa.priority(fa.priority.is - 2).save
                        FileArticle.fixPriorities(article)
                         S.redirectTo(Site.addArticleFileLoc.calcHref(article))
                      }) } </td> ++
                      <td> {
                        SHtml.text(fa.priority.is.toString, s => {
                          for {
                            pri <- asInt(s)
                          } yield {
                            fa.priority(pri).save
                          }
                        })
                      } </td> ++
                      <td> { SHtml.button(Text("Down") , ()=> {
                        fa.priority(fa.priority.is + 2).save
                        FileArticle.fixPriorities(article)
                         S.redirectTo(Site.addArticleFileLoc.calcHref(article))
                      }) } </td> ++
                      <td> {
                        SHtml.ajaxButton(Text("Delete"), () => {
                          fa.delete_!
                          RedirectTo(Site.addArticleFileLoc.calcHref(article))

                        })
                      }</td>
                  }</tr>
              }.getOrElse(NodeSeq.Empty)
          }
      } </table>

      submit = ("#submit" #> SHtml.button(Text("Submit"), () => {
        FileArticle.fixPriorities(article)
        S.redirectTo(Site.addArticleFileLoc.calcHref(article))
      })).apply(in)
    }

    out ++ submit
  }

  def addFile(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      article <- Site.addArticleFileLoc.currentValue
    } yield {
      val fileTypes = FileType.mapped.map(m => (m._1.toString, m._2)).toSeq
      var ft = fileTypes.head._1
      var name = ""
      var priority = ""

      out = (

        "#name" #> SHtml.text("", name = _) &
        "#upload" #> SHtml.fileUpload(ul => theUpload(Full(ul))) &
        "#priority" #> SHtml.text(priority, priority = _) &
        "#chooseFileTypes" #> SHtml.select(fileTypes, Full("1"), s => {
          ft = s
        }) &
        "type=submit" #> SHtml.onSubmitUnit(() => {
          if (!MyErrors.checkLimitError(name, "name", 220)) {

            if (priority.isEmpty || asInt(priority).isEmpty) {
              priority = (article.getFiles.size + 1).toString
            }
            for {
              file <- theUpload
              ftInt <- asInt(ft)
              prInt <- asInt(priority)
              fileObj <- AvvisoFile.add(file.fileName, name, FileType.apply(ftInt))

            } yield {
              val ftype = FileType.mapped(ftInt)
              FileArticle.add(article, fileObj, prInt)
              if (ftype == "Photo") {
                val rfBI = ImageHelpers.resize(file.fileStream, 140, 130)
                ImageHelpers.saveBI(AvvisoOption.getGlobalFilePath + fileObj.id.get + "_140x130", rfBI)
                FileHelpers.save(file.fileStream, AvvisoOption.getGlobalFilePath + fileObj.id.get)
              } else {
                FileHelpers.save(file.fileStream, AvvisoOption.getGlobalFilePath + fileObj.id.get)
              }
              FileArticle.fixPriorities(article)
              theUpload(Empty)

              S.redirectTo(Site.addArticleFileLoc.calcHref(article))
            }

          }
        }) &
        "#cancel" #> SHtml.button(Text("Cancel"), () => {
          S.redirectTo(Site.crudArticles.fullUrl)
        })).apply(in)

    }
    out
  }

}

class ChooseFilesForArticle {

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      article <- Site.addArticleFileLoc.currentValue
    } yield {
      val filesAlreadyIn = article.getFiles.map { case (fa, f) => f.map { _.id.is }.getOrElse[Long](-1) }.filter(_ != -1)
      var i = filesAlreadyIn.size + 1
      out = ("#listOfFiles" #> <table id="listOfFiles"> {
        <tr>
          <th> Name </th>
          <th> Check </th>
        </tr> ++
          AvvisoFile.findAll.filter {
            af => !filesAlreadyIn.exists(i => { i == af.id.is })
          }.map {
            af =>
              <tr> {
                <td> { af.name.is } </td> ++
                  <td> {
                    SHtml.checkbox(false, b => {
                      if (b) {
                        FileArticle.add(article, af, i)
                        i += 1
                      }
                    })
                  } </td>
              }</tr>
          }
      }</table> &
        "#submit" #> SHtml.button(Text("Submit"), () => {
          S.redirectTo(Site.addArticleFileLoc.calcHref(article))
        })).apply(in)

    }
    out
  }

}