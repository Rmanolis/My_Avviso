package code.lib
import net.liftweb.http._
import net.liftweb.http.rest._
import net.liftweb.json.JsonAST._
import code.model._


object ArticleRest extends RestHelper{
  case class Geolocation(title:String,lat:Double,lon:Double)
   serve {
      case JsonGet("api" :: "getArticleLocations" :: id :: _, _) => {
        var list:List[Geolocation]=List()
        for{
          art <- Article.find(id)
        }yield{
          GeolocateArticle.findByArticle(art).map{
            ga =>
              list +:= Geolocation(ga.explanation.is,ga.lat.is,ga.lon.is)
          }
        }
        
        JArray(list.map{
          g => 
            JObject(List(
            		JField("title",JString(g.title)),
            		JField("lat" , JDouble(g.lat)),
            		JField("lon" , JDouble(g.lon))
            ))
        })   
      }
     
   }
  

}