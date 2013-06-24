package shalene

import java.io.{File, FileInputStream}
import java.util.ArrayList
import ij.process.ImageProcessor
import loci.formats.{ChannelSeparator, FormatException, IFormatReader}
import loci.plugins.util.{ImageProcessorReader, LociPrefs}


/** Loads stacked TIFFs and returns a `TiffStack[T]` object.
  * @author James R. Thompson, D.Phil
  * @since 11/16/12
  */
object ImageLoad {

  def loadTIFF(file: File) = {
    val td = new TiffDecoder(file)
    val fi = td.getTiffInfo.get(0)
    println("Bit depth is : " + fi.getType)
    fi.getType match {
      case "byte" => loadImage[Byte](fi, file, 0xff)
      case "short" => loadImage[Short](fi, file, 0xffff)
      case "ushort" => loadImage[Short](fi, file, 0xffff)
      case "int" => loadImage[Int](fi, file)
      case "uint" => loadImage[Int](fi, file)
      case "float" => loadImage[Float](fi, file)
      case "double" => loadImage[Double](fi, file)
    }
  }

  def loadND2(file: File) = {
    val reader = new ImageProcessorReader(new ChannelSeparator(LociPrefs.makeImageReader))
    reader.setId(file.getAbsolutePath)
    val numFrames = reader.getImageCount
    val width = reader.getSizeX
    val height = reader.getSizeY
    val bitDepth = reader.getBitsPerPixel
    println(s"Number of bits per pixel = $bitDepth")
    val mask = if(bitDepth == 8) 0xff else 0xffff
    val out = for(i <- 0 until numFrames) yield bitMask(reader.openProcessors(i)(0).getPixels.asInstanceOf[Array[Short]], width, height, mask)
    new TiffStack[Int](out)
  }

  private def bitMask[T: Numeric : Manifest](xs: Array[T], width: Int, height: Int, mask: Int) : NumericImage[Int] = {
    val ev = implicitly[Numeric[T]]
    new NumericImage[Int](width, height, xs.view.map(ev.toInt(_)&mask).toArray)
  }

  private def loadImage[T: Numeric : Manifest](fileData: FileInfo, file: File, mask: Int = 0) : TiffStack[Int] = {
    val is = new FileInputStream(file)
    val reader = new Reader(fileData)
    var skip = fileData.getOffset
      val out = for(i <- 0 until fileData.nImages) yield {
        val pixels = reader.readPixels(is, skip)
        skip = fileData.gapBetweenImages
        bitMask(pixels.asInstanceOf[Array[T]], fileData.width, fileData.height, mask)
      }
    is.close()
    new TiffStack[Int](out)
  }


}