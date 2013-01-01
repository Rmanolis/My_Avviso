package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib._
import net.liftweb.http.S

class Advert extends LongKeyedMapper[Advert] with IdPK {
  def getSingleton = Advert
  object company extends MappedString(this, 220) {
    override def validations = valMinLen(1, "Company's name cannot be blank") _ ::
      valMaxLen(220, "Company's name is too long") _ ::
      super.validations
  }
  object productName extends MappedString(this, 220) {
    override def validations = valMinLen(1, "Product's name cannot be blank") _ ::
      valMaxLen(220, "Product's name is too long") _ ::
      super.validations
  }
  object telephone extends MappedString(this, 220) {
    override def validations = valMinLen(1, "Telephone cannot be blank") _ ::
      valMaxLen(220, "Telephone is too long") _ ::
      super.validations
  }
  object email extends MappedString(this, 220) {
    override def validations = valMinLen(1, "Email cannot be blank") _ ::
      valMaxLen(220, "Email is too long") _ ::
      valEmail("This is not an email") _ ::
      super.validations
  }
  object introduction extends MappedText(this)
  object language extends MappedLongForeignKey(this, Language)
  object html extends MappedText(this)

  def valEmail(errorMsg: ⇒ String)(name: String): List[FieldError] = {
    name.contains("@") match {
      case true => Nil
      case false => FieldError(this.email, errorMsg) :: Nil
    }
  }
  def edit(lan: Language, com: String, pro: String, tel: String, email: String, intro: String, html: String) = {
    this.language(lan).company(com).productName(pro).introduction(intro).html(html).email(email).telephone(tel)
    this.validate match {
      case Nil => this.save; Full(this)
      case xs => S.error(xs); Empty
    }
  }

  def getLanguageName = this.language.obj.map(_.name.is).getOrElse("")
}

object Advert extends Advert with LongKeyedMetaMapper[Advert] {
  def add(lan: Language, com: String, pro: String, tel: String, email: String, intro: String, html: String) = {
    val a = Advert.create
    a.language(lan).company(com).productName(pro).introduction(intro).email(email).telephone(tel)
    a.validate match {
      case Nil => a.save; Full(a)
      case xs => S.error(xs); Empty
    }
  }
}