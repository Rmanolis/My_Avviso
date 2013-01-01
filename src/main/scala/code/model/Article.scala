package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import code.lib._
import scala.xml.NodeSeq
import net.liftweb.http.S
class Article extends LongKeyedMapper[Article] with IdPK {
  def getSingleton = Article
  object title extends MappedString(this, 300) {
    override def validations = valMinLen(1, "Title cannot be blank") _ ::
      valMaxLen(300, "Title is too long") _ ::
      super.validations
  }
  object category extends MappedLongForeignKey(this, Category)
  object isDraft extends MappedBoolean(this)
  object article extends MappedText(this)
  object dateTime extends MappedDateTime(this)

  def edit(cat: Category, title: String, article: String, isDraft: Boolean) = {

    this.title(title).category(cat).article(article).isDraft(isDraft).dateTime(Helpers.now)
    this.validate match {
      case Nil => this.save; Full(this)
      case xs => S.error(xs); Empty
    }
  }

  def getCategoryName = {
    this.category.obj.map(_.name.is).getOrElse("")
  }

  def delete {
    CommentArticle.findByArticle(this).map {
      ca => ca.delete_!
    }
    GeolocateArticle.findByArticle(this).map {
      ca => ca.delete_!
    }
    FileArticle.findByArticle(this).map{
      fa => fa.delete_!
    }
    
    this.delete_!
  }

  def getLocations = {
    GeolocateArticle.findByArticle(this)
  }

  def getSummary = {
    MyHelpers.htmlToString(this.article.is)
  }

  def getFiles: List[(FileArticle,Box[AvvisoFile])] = {
    FileArticle.findAll(By(FileArticle.article, this.id.is), OrderBy(FileArticle.priority, Ascending)).map {
      fa => (fa,fa.file.obj)
    }
  }

  def getSmallPictureHtml :NodeSeq = {
    FileArticle.findAll(By(FileArticle.article, this.id.is), OrderBy(FileArticle.priority, Ascending)).headOption.map {
      fa =>
        fa.file.map {
          file =>
            if (FileType.mapped(file.fileType.get.id) == "Photo") {
              <a href={Site.chooseArticleLoc.calcHref(this)}><img src={ "/files/" + file.id.is+"_140x130" } alt={ file.name.is } width="140" height="130"></img></a>
            } else {
              NodeSeq.Empty
            }
        }.getOrElse(NodeSeq.Empty)
    }.getOrElse(NodeSeq.Empty)
  }

}
object Article extends Article with LongKeyedMetaMapper[Article] with CRUDify[Long,Article] {
  def add(cat: Category, title: String, article: String, isDraft: Boolean) = {
    val c = Article.create
    c.title(title).category(cat).article(article).isDraft(isDraft).dateTime(Helpers.now).save
    c.validate match {
      case Nil => c.save; Full(c)
      case xs => S.error(xs); Empty
    }
  }

  def quickAdd(title: String, article: String, isDraft: Boolean) = {
    val c = Article.create
    val s = c.title(title).article(article).isDraft(isDraft).dateTime(Helpers.now).save
    if (s) {
      Full(c)
    } else {
      Empty

    }
  }

  def findByLanguage(language: Language) = {
    Category.findAll(By(Category.language, language)).map {
      c =>
        Article.findAll(By(Article.category, c.id.is), OrderBy(Article.dateTime, Descending))
    }.flatten(itr => itr)
  }

  def findByName(title: String) = {
    Article.find(By(Article.title, title))
  }

  def findByCategory(cat: Category) = {
    Article.findAll(By(Article.category, cat.id.is), OrderBy(Article.dateTime, Descending))
  }
}
