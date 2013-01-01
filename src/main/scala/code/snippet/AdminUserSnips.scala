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

object CreateUser extends LiftScreen {
  object person extends ScreenVar(User.create)

  override def screenTop =
    <b>A single screen with some input validation</b>

  addFields(() => person.is.firstName)
  addFields(() => person.is.lastName)
  addFields(() => person.is.email)
  addFields(() => person.is.password)

  def finish() {
    S.notice("Thank you for adding " + person.is)

    person.is.save

  }
}

class CRUDUsers {
  def render = {
    <table id="listOfUser"> {
      <tr>
        <th>Email</th>
        <th>Admin </th>
        <th>Delete</th>
      </tr> ++
        User.findAll.map {
          u =>
            <tr> {
              <td> { u.email.is } </td> ++
                <td> {
                  val action = if (u.superUser.is) { Text("Remove as Admin") } else { Text("Approve as Admin") }
                  SHtml.button(action, () => {
                    if (u.superUser.is) {
                      u.superUser(false).save
                    } else {
                      u.superUser(true).save
                    }
                    S.redirectTo(Site.crudUsers.fullUrl)
                  })
                } </td> ++
                <td> {
                  SHtml.button(Text("Delete"), () => {
                    u.delete_!
                    S.redirectTo(Site.crudUsers.fullUrl)
                  })
                } </td>
            }</tr>
        }
    }</table>
  }
}
