package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S

class Category extends LongKeyedMapper[Category] with IdPK with Logger {
  def getSingleton = Category
  object name extends MappedString(this, 300) {
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(300, "Name is too long") _ ::
      super.validations
  }
  object language extends MappedLongForeignKey(this, Language)
  object parent extends MappedLongForeignKey(this, Category)
  def edit(lang: Language, name: String) = {

    this.name(name).language(lang)
    this.validate match {
      case Nil => this.save; Full(this)
      case xs => S.error(xs); Empty
    }
  }

  def getLanguageName = {
    language.obj match {
      case Full(l) => l.name.is
      case _ => error("It did nt have a language"); ""
    }
  }

  def addParent(cat: Category) = {
    this.parent(cat).save
  }

  def removeParent = {
    this.parent(Empty).save
  }

  def listOfChilds = {
    Category.findAll(By(Category.parent, this.id.is))
  }

  def deleteDependents{
    Article.findByCategory(this).map {
      a => a.delete
    }
  }
  def cleanDependents{
    Article.findByCategory(this).map {
      a => a.category(this).save
    }
  }
  def delete {
    cleanDependents
    this.delete_!
  }
}

object Category extends Category with LongKeyedMetaMapper[Category] with Logger {
  def add(language: Language, name: String) = {
    val c = Category.create
    c.language(language).name(name)
    c.validate match {
      case Nil => c.save; Full(c)
      case xs => S.error(xs); Empty
    }
  }

  def findByLanguage(language: Language) = {
    Category.findAll(By(Category.language, language))
  }
  def findByName(name: String) = {
    Category.find(By(Category.name, name))
  }
}

object DefaultCategory {
  def save {
    import Category._
    for {
      lang <- Language.findByName("Ελληνικά")
    } yield {
      if (findByName("Πολιτική").isEmpty) {
        add(lang, "Πολιτική")
      }
      if (findByName("Οικονομία").isEmpty) {
        add(lang, "Οικονομία")
      }

    }
    
    for {
      lang <- Language.findByName("English")
    } yield {
      if (findByName("Politics").isEmpty) {
        add(lang, "Politics")
      }
      if (findByName("Economy").isEmpty) {
        add(lang, "Economy")
      }

    }
  }
}
