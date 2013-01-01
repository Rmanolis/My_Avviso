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

class GetLabel {

  def render(in: NodeSeq): NodeSeq = {
    var out = NodeSeq.Empty
    for {
      lan <- MyHelpers.getLanguage
      ll <- Localization.findByLabelAndLanguage(in.text.trim, lan)
    } yield {
    	out = ("#label" #> ll.text.is).apply(in)
    }
    out
  }
}