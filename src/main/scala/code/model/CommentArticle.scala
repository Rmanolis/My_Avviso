package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S

class CommentArticle extends LongKeyedMapper[CommentArticle] with IdPK  with Logger{
  def getSingleton = CommentArticle
  object email extends MappedString(this, 200) {
    override def validations = valMinLen(1, "email cannot be blank") _ ::
      valMaxLen(200, "email is too long") _ ::
      super.validations
  }
  object name extends MappedString(this, 200){
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(200, "Name is too long") _ ::
      super.validations
  }
  object website extends MappedString(this, 200){
    override def validations = valMinLen(1, "website cannot be blank") _ ::
      valMaxLen(200, "website is too long") _ ::
      super.validations
  }
  object message extends MappedText(this)

  object article extends MappedLongForeignKey(this, Article)
  
  def edit(article:Article, email:String, name:String,website:String,message:String)={
    this.article(article).email(email).name(name).website(website).message(message)
    this.validate match {
      case Nil => this.save; Full(this)
      case xs => S.error(xs) ; Empty
    }
  }
}

object CommentArticle extends CommentArticle with LongKeyedMetaMapper[CommentArticle] with Logger{

  def add(article:Article, email:String, name:String,website:String,message:String)={
    val ca = CommentArticle.create
    ca.article(article).email(email).name(name).website(website).message(message)
    ca.validate match {
      case Nil => ca.save; Full(ca)
      case xs => S.error(xs) ; Empty
    }
    
  }
  
  def findByArticle(article:Article)={
    CommentArticle.findAll(By(CommentArticle.article,article.id.is))
  }
}

