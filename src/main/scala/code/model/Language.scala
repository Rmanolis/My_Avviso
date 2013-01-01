package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S

class Language extends LongKeyedMapper[Language] with IdPK {
  def getSingleton = Language
  object name extends MappedString(this, 140) {
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(140, "Name is too long") _ ::
      super.validations
  }
  object isDefault extends MappedBoolean(this)
  def edit(name: String, default: Boolean) = {
    if (default) {
      Language.findAll.map {
        lan =>
          lan.isDefault(false).save
      }
    }
    this.name(name).isDefault(default)
    this.validate match {
      case Nil => this.save; Full(this)
      case xs => S.error(xs); Empty
    }
  }

  def cleanDependents{
    Category.findByLanguage(this).map {
      c => c.language(Empty).save
    }
  }
  def deleteDependents{
    Category.findByLanguage(this).map {
      c => c.language(Empty).save
    }
  }
  def delete = {
    cleanDependents
    delete_!
  }
}

object Language extends Language with LongKeyedMetaMapper[Language] {
  def add(name: String, default: Boolean) = {
    val l = Language.create
    if (default) {
      Language.findAll.map {
        lan =>
          lan.isDefault(false).save
      }
    }
    l.name(name).isDefault(default)
    l.validate match {
      case Nil => l.save; Full(l)
      case xs => S.error(xs); Empty
    }

  }
  def findByName(title: String) = {
    Language.find(By(Language.name, title))
  }

  def findDefault = {
    Language.find(By(Language.isDefault, true))
  }
}


object DefaultLanguage {
  def save{
    import Language._
    if(findByName("Ελληνική").isEmpty){
      add("Ελληνική",true)
    }
     if(findByName("English").isEmpty){
      add("English",false)
    }
  }
}