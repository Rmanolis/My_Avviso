package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._


class FileArticle extends LongKeyedMapper[FileArticle] with IdPK  {
  def getSingleton = FileArticle
  object article extends MappedLongForeignKey(this, Article)
  object file extends MappedLongForeignKey(this, AvvisoFile)
  object priority extends MappedInt(this)
  def edit(article:Article , file : AvvisoFile, priority:Int)={
    this.article(article).file(file).priority(priority).save
  }

  
  

}


object FileArticle extends FileArticle  with LongKeyedMetaMapper[FileArticle] {
  def add(article:Article , file : AvvisoFile, priority:Int)={
    val fa = FileArticle.create
    fa.file(file).article(article).priority(priority).save
    fa
  }
  
  def findByArticle(article:Article)={
    FileArticle.findAll(By(FileArticle.article, article.id.is))
  }
  def fixPriorities(art:Article)={
    var i = 1
    FileArticle.findAll(By(FileArticle.article,art.id.is), OrderBy(FileArticle.priority, Ascending)).map{
      fa => 
        fa.priority(i).save
        i += 1  
        fa
    }
  }
}