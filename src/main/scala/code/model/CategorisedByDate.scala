package code.model
/*import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class CategorisedByDate extends LongKeyedMapper[CategorisedByDate] with IdPK {
  def getSingleton = CategorisedByDate
  object  extends MappedString(this, 140)
  def edit(lang : Language,name: String) = {
    
    this.name(name).language(lang).save
  }
}
object CategorisedByDate extends CategorisedByDate with LongKeyedMetaMapper[CategorisedByDate] with Loggable{
   def add(language: Language, name: String) = {
    val c = Category.create
    c.language(language).name(name).save()
    c
  }
  

}*/
