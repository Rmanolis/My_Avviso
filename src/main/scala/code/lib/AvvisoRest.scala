package code.lib
import net.liftweb.http.JsonResponse
import net.liftweb.http.rest.RestHelper
import net.liftweb.http.{ InMemoryResponse, StreamingResponse }
import net.liftweb.http.S
import net.liftweb.http.FileParamHolder
import net.liftweb.json.JsonAST.JValue
import net.liftweb.json.JsonDSL._
import net.liftweb.common.{ Box, Full }
import net.liftweb.http.BadResponse
import net.liftweb.util.StringHelpers
import code.model._
import java.io._
import org.apache.commons.io.IOUtils.copy

object AvvisoRest extends RestHelper {
  serve {

    case "files" :: imageName :: Nil Get req => {
      
      val file = new File(AvvisoOption.getGlobalFilePath + imageName)
      
      val inputStream = new FileInputStream(file)
      val fileName = AvvisoFile.find(imageName).map(_.dir.is).getOrElse(imageName)
      val headers: List[(String, String)] =
        List(
          ("Content-disposition", "attachment; charset=utf-8; filename=" + fileName.split(" ").mkString  ))

      StreamingResponse(inputStream, () => inputStream.close(), file.length, headers, Nil, 200)

    }
  }
}