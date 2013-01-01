package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._

class GeolocateArticle extends LongKeyedMapper[GeolocateArticle] with IdPK  with Logger{
  def getSingleton = GeolocateArticle
  object explanation extends MappedText(this)
  object article extends MappedLongForeignKey(this, Article)
  object lat extends MappedDouble(this)
  object lon extends MappedDouble(this)
  def edit(article:Article, explanation:String, lat:Double,lon:Double)={
    this.article(article).explanation(explanation).lat(lat).lon(lon).save
  }
}

object GeolocateArticle extends GeolocateArticle with LongKeyedMetaMapper[GeolocateArticle] with Logger{

  def add(article:Article, explanation:String, lat:Double,lon:Double)={
    val ga = GeolocateArticle.create
    ga.article(article).explanation(explanation).lat(lat).lon(lon).save
    ga
  }
  
  def findByArticle(article:Article)={
    GeolocateArticle.findAll(By(GeolocateArticle.article,article.id.is))
  }
}