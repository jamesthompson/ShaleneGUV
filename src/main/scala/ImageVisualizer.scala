package shalene

import java.awt.image._
import javafx.scene.image.{Image => JfxImg}
import javax.imageio.ImageIO
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException}
import java.util.logging.{Level, Logger}

trait ImageVisualizer[@specialized(Byte, Short, Int, Float, Double) T] {
  def visualize(img: NumericImage[T]) : BufferedImage
  def getJavaFXImage(img: NumericImage[T], width: Int, height: Int) : JfxImg
}

object ImageVisualizer {

  trait NumericImageVisualizer[@specialized(Byte, Short, Int, Float, Double) N] extends ImageVisualizer[N] {
    implicit def ev: Numeric[N]

    def visualize(img: NumericImage[N]) = {
      val byteArray : Array[Byte] = img.normalizedPixels.map(_.toByte)
      createBufferedImage(byteArray, img.width, img.height)
    }

    // Regardless of underlying NumericImage bitdepth, this will produce a perfect JavaFX Image representation.
    def getJavaFXImage(img: NumericImage[N], width: Int, height: Int) = {
      val byteArray : Array[Byte] = img.getBuffer.map(ev.toInt(_).toByte)
      makeJFXImage(byteArray, width, height)
    }

    def makeJFXImage(rawPixels: Array[Byte], width: Int, height: Int) = {
      val out = new ByteArrayOutputStream
      try {
        ImageIO.write(createBufferedImage(rawPixels, width, height).asInstanceOf[RenderedImage], "png", out)
        out.flush
      } catch {
        case ex : Exception => Logger.getLogger("JavaFX Image Handling Exception!").log(Level.SEVERE, null, ex)
      }
      new JfxImg(new ByteArrayInputStream(out.toByteArray))
    }

    def createBufferedImage(pixels:Array[Byte], width:Int, height:Int) = {
      def getDefaultColorModel = {
        val r  = new Array[Byte](256)
        val g  = new Array[Byte](256)
        val b  = new Array[Byte](256)
        for(i <- 0 to 255) {
          r(i) = i.toByte
          g(i) = i.toByte
          b(i) = i.toByte
        }
        new IndexColorModel(8, 256, r, g, b) 
      }
      def getIndexSampleModel(width:Int, height:Int) = {
        val icm = getDefaultColorModel
        val wr = icm.createCompatibleWritableRaster(1, 1)
        wr.getSampleModel.createCompatibleSampleModel(width, height)
      }
      val sm = getIndexSampleModel(width, height)
      val db = new DataBufferByte(pixels, width * height, 0)
      val raster = Raster.createWritableRaster(sm, db, null)
      new BufferedImage(getDefaultColorModel, raster, false, null)
    }

  }
  
  implicit def numericViz[N : Numeric]: ImageVisualizer[N] = new NumericImageVisualizer[N] {
    def ev = implicitly[Numeric[N]]
  }

  def visualize[T](img: NumericImage[T])(implicit viz: ImageVisualizer[T]) = viz.visualize(img)
  def getJavaFXImage[T](img: NumericImage[T], width: Int, height: Int)(implicit viz: ImageVisualizer[T]) = viz.getJavaFXImage(img, width, height)
}
