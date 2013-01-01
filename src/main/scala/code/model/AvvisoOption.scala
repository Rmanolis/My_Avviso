package code.model
import net.liftweb.mapper._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S

class AvvisoOption extends LongKeyedMapper[AvvisoOption] with IdPK {
  def getSingleton = AvvisoOption
  object nameForProgrammer extends MappedString(this, 140) {
    override def validations = valMinLen(1, "Name for programmer cannot be blank") _ ::
      valMaxLen(140, "Name for programmer is too long") _ ::
      super.validations
  }
  object nameForViewer extends MappedString(this, 300) {
    override def validations = valMinLen(1, "Name for viewer cannot be blank") _ ::
      valMaxLen(140, "Name for viewer is too long") _ ::
      super.validations
  }
  object value extends MappedText(this)
  def edit(nameForProgrammer: String, nameForViewer: String, value: String) = {
    this.nameForViewer(nameForViewer).nameForProgrammer(nameForProgrammer).value(value)
    this.validate match {
      case Nil => this.save; Full(this)
      case xs => S.error(xs); Empty
    }
  }
}

object AvvisoOption extends AvvisoOption with LongKeyedMetaMapper[AvvisoOption] {
  def add(nameForProgrammer: String, nameForViewer: String, value: String) = {
    val o = AvvisoOption.create
    o.nameForProgrammer(nameForProgrammer).nameForViewer(nameForViewer).value(value)
    o.validate match {
      case Nil => o.save; Full(o)
      case xs => S.error(xs); Empty
    }
  }

  def findByNameForProgrammer(name: String) = {
    AvvisoOption.find(By(AvvisoOption.nameForProgrammer, name))
  }

  def getGlobalFilePath = {
    findByNameForProgrammer("GlobalFilePath").map {
      v =>
        v.value.is
    }.getOrElse("src/main/webapp/files/")
  }
}

object DefaultAvvisoOption {
  def save {
    if (AvvisoOption.findByNameForProgrammer("GlobalFilePath").isEmpty) {
      AvvisoOption.add("GlobalFilePath", "where would you like to save the files ?", "src/main/webapp/files/")
    }
    if (AvvisoOption.findByNameForProgrammer("VideoIcon").isEmpty) {
      AvvisoOption.add("VideoIcon", "the icon for video", "Video_icon.png")
    }
    if (AvvisoOption.findByNameForProgrammer("TextIcon").isEmpty) {
      AvvisoOption.add("TextIcon", "the icon for text", "Text_icon.png")
    }
    if (AvvisoOption.findByNameForProgrammer("AudioIcon").isEmpty) {
      AvvisoOption.add("AudioIcon", "the icon for audio", "Audio_icon.jpg")
    }
    if (AvvisoOption.findByNameForProgrammer("OtherIcon").isEmpty) {
      AvvisoOption.add("OtherIcon", "the icon for other", "Other_icon.png")
    }
  }
}