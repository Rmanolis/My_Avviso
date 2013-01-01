package code.lib
import _root_.net.liftweb._
import http._
import js._
import JsCmds._
import common._
import json._

import scala.xml._
/**
* Respond to JSON requests in a stateless dispatch
*/
object StatelessJson {
  def init() {
    // register the JSON handler
    LiftRules.statelessDispatch.append{
      case r @ Req("stateless_json_call" :: Nil, _, PostRequest) => () => handleJson(r)
    }
  }

  implicit def iterableToBox[X](in: Iterable[X]): Box[X] = in.toList.headOption

  def handleJson(req: Req): Box[LiftResponse] =
  for {
    json <- req.json // get the JSON
    JObject(List(JField("command", JString(cmd)), JField("params", JString(params)))) <- json // extract the command
  } yield JavaScriptResponse(SetHtml("json_result",cmd match { // build the response
        case "show" => Text(params)
        case x => <b>Problem... didn't handle JSON message {x}</b>
      }))
}