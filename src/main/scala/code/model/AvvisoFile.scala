package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import scala.xml.NodeSeq
import net.liftweb.http.S

object FileType extends Enumeration {
  val Photo = new Val(1, "Photo")
  val Text = new Val(2, "Text")
  val Music = new Val(3, "Audio")
  val Video = new Val(4, "Video")
  val Other = new Val(5, "Other")
  val mapped = Map((1 -> "Photo"), (2 -> "Text"), (3 -> "Audio"), (4 -> "Video"), (5 -> "Other"))
  def filePathIconForVideo = {
    AvvisoOption.findByNameForProgrammer("VideoIcon").map {
      ao =>
        "/images/" + ao.value.is
    }.getOrElse("")
  }
  def filePathIconForText = {
    AvvisoOption.findByNameForProgrammer("TextIcon").map {
      ao =>
        "/images/" + ao.value.is
    }.getOrElse("")
  }
  def filePathIconForAudio = {
    AvvisoOption.findByNameForProgrammer("AudioIcon").map {
      ao =>
        "/images/" + ao.value.is
    }.getOrElse("")
  }
  def filePathIconForOther = {
    AvvisoOption.findByNameForProgrammer("OtherIcon").map {
      ao =>
        "/images/" + ao.value.is
    }.getOrElse("")
  }
}
class AvvisoFile extends LongKeyedMapper[AvvisoFile] with IdPK {
  def getSingleton = AvvisoFile
  object name extends MappedString(this, 220) {
    override def validations = valMinLen(1, "Name cannot be blank") _ ::
      valMaxLen(220, "Name is too long") _ ::
      super.validations
  }
  object dir extends MappedString(this, 400) {
    override def validations = valMinLen(1, "Directory cannot be blank") _ ::
      valMaxLen(400, "Directory is too long") _ ::
      super.validations
  }
  object fileType extends MappedEnum(this, FileType)
  def edit(dir: String, name: String, ft: FileType.Value) = {
    this.dir(dir).name(name).fileType(ft)
    this.validate match {
      case Nil => this.save; Full(this)
      case xs => S.error(xs); Empty
    }
  }

  def getImgHtml(width: Int, height: Int) = {
    FileType.mapped(this.fileType.get.id) match {
      case "Photo" => <img src={ "/files/" + id.is } alt={ this.name.is } width={ width.toString } height={ height.toString }></img>
      case "Text" => <a href={ "/files/" + id.is }><img src={ FileType.filePathIconForText } alt={ this.name.is } width={ width.toString } height={ height.toString }></img></a>
      case "Video" => <a href={ "/files/" + id.is }><img src={ FileType.filePathIconForVideo } alt={ this.name.is } width={ width.toString } height={ height.toString }></img></a>
      case "Audio" => <a href={ "/files/" + id.is }><img src={ FileType.filePathIconForAudio } alt={ this.name.is } width={ width.toString } height={ height.toString }></img></a>
      case "Other" => <a href={ "/files/" + id.is }><img src={ FileType.filePathIconForOther } alt={ this.name.is } width={ width.toString } height={ height.toString }></img></a>
    }

  }

  def delete = {
	 import java.io._
	 val file = new File(AvvisoOption.getGlobalFilePath + this.id.get.toString)
	 if(this.fileType.is.id == 1) //check if it is photo
	 {
	   val img = new File(AvvisoOption.getGlobalFilePath + this.id.get.toString + "_140x130")
	   img.delete
	 }
	 file.delete()
	 this.delete_!
  }

}

object AvvisoFile extends AvvisoFile with LongKeyedMetaMapper[AvvisoFile] {
  def add(dir: String, name: String, ft: FileType.Value) = {
    val a = AvvisoFile.create
    a.dir(dir).name(name).fileType(ft)
    a.validate match {
      case Nil => a.save; Full(a)
      case xs => S.error(xs); Empty
    }
  }
}
