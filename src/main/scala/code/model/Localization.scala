package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S

class Localization extends LongKeyedMapper[Localization] with IdPK {
  def getSingleton = Localization
  object label extends MappedString(this, 140) {
    override def validations = valMinLen(1, "Label cannot be blank") _ ::
      valMaxLen(140, "Label is too long") _ ::
      super.validations
  }
  object text extends MappedText(this)
  object language extends MappedLongForeignKey(this, Language)

  def edit(label: String, value: String, language: Language) = {
    val l = this.label(label.trim).text(value).language(language)
    l.validate match {
      case Nil => l.save; Full(l)
      case xs => S.error(xs); Empty
    }
  }

  def getLanguageName = {
    this.language.obj match {
      case Full(lang) => {
        lang.name.is
      }
      case Empty => {

        ""
      }
      case Failure(m, n, s) => {
        ""
      }
    }
  }
}

object Localization extends Localization with LongKeyedMetaMapper[Localization] {
  def add(language: Language, label: String, value: String) = {
    val l = Localization.create
    l.label(label.trim).text(value).language(language)
    l.validate match {
      case Nil => l.save; Full(l)
      case xs => S.error(xs); Empty
    }
  }

  def findByLabel(title: String) = {
    Localization.findAll(By(Localization.label, title))
  }

  def findByLabelAndLanguage(label: String, language: Language) = {
    Localization.findAll(BySql("label = ? AND language_c = ?",
      IHaveValidatedThisSQL("mragias", "2012-11-02"),
      label, language.id.is)).headOption

  }

}

object DefaultLocalization {
  import Localization._
  def save {
    for {
      lang <- Language.findByName("Ελληνικά")

    } yield {
      if (findByLabelAndLanguage("LatestNewsByCategory", lang).isEmpty) {
        add(lang, "LatestNewsByCategory", "Νέα ανα κατηγορία")
      }
       if (findByLabelAndLanguage("Intro", lang).isEmpty) {
        add(lang, "Intro", "MyAvviso είναι CMS για να ανεβάζεις τα άρθρα σου ")
      }
       if (findByLabelAndLanguage("ListOfCategories", lang).isEmpty) {
        add(lang, "ListOfCategories", "Κατηγορίες")
      }
       if (findByLabelAndLanguage("ListOfLanguages", lang).isEmpty) {
        add(lang, "ListOfLanguages", "Γλώσσες")
      }
    }

    for {
      lang <- Language.findByName("English")
    } yield {
      if (findByLabelAndLanguage("LatestNewsByCategory", lang).isEmpty) {
        add(lang, "LatestNewsByCategory", "News by category")
      }
      if (findByLabelAndLanguage("Intro", lang).isEmpty) {
        add(lang, "Intro", "MyAvviso is CMS to post news ")
      }
       if (findByLabelAndLanguage("ListOfCategories", lang).isEmpty) {
        add(lang, "ListOfCategories", "Categories")
      }
       if (findByLabelAndLanguage("ListOfLanguages", lang).isEmpty) {
        add(lang, "ListOfLanguages", "Languages")
      }
    }
  }
}
