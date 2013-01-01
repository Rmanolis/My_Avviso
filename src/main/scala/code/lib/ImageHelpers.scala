package code.lib
import java.awt.Image
import java.awt.image.BufferedImage
import javax.imageio.ImageIO
import java.awt.Graphics2D
import java.awt.AlphaComposite
import java.io._

object ImageHelpers {
 def resize(is: InputStream, maxWidth:Int, maxHeight:Int):BufferedImage = {
    require (maxWidth > 0)
    require (maxHeight > 0)
    val originalImage:BufferedImage = ImageIO.read(is)

    var height = originalImage.getHeight
    var width = originalImage.getWidth

    // Shortcut to save a pointless reprocessing in case the image is small enough already
    if (width <= maxWidth && height <= maxHeight)
        originalImage
    else {          
        // If the picture was too big, it will either fit by width or height.
        // This essentially resizes the dimensions twice, until it fits
        if (width > maxWidth){
          height = (height.doubleValue() * (maxWidth.doubleValue() / width.doubleValue())).intValue
          width = maxWidth
        }
        if (height > maxHeight){
          width = (width.doubleValue() * (maxHeight.doubleValue() / height.doubleValue())).intValue
          height = maxHeight
        }
        val scaledBI = new BufferedImage(width, height,  BufferedImage.TYPE_INT_RGB)
        val g = scaledBI.createGraphics
        g.setComposite(AlphaComposite.Src)
        g.drawImage(originalImage, 0, 0, width, height, null);
        g.dispose
        scaledBI
    }
 }
 
 def saveBI(fileName:String,bi: BufferedImage )={
    val outputfile = new File(fileName);
    ImageIO.write(bi, "jpg", outputfile);
 }
 
}

object FileHelpers{
 import org.apache.commons.io.IOUtils.copy
 def save(is: InputStream ,fileName:String)={
   val dest = new File(fileName)
   copy(is, new FileOutputStream(dest))
 }
}