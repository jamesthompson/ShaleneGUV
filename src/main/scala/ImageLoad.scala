package shalene

import ij.process.ImageProcessor
import java.io.{File, FileInputStream}
import java.lang.Float
import loci.formats.{ChannelSeparator, FormatException, IFormatReader}
import loci.plugins.util.{ImageProcessorReader, LociPrefs}

object ImageLoad {

	def loadTIFF(file: File) : TiffStack[Int] = {
		val td = new TiffDecoder(file)
    val fi = td.getTiffInfo.get(0)
    val in = new FileInputStream(file)
    val reader = new ImageReader(fi)
    val stack = for(i <- 0 until fi.nImages) yield new NumericImage[Int](fi.width, fi.height, fi.fileType match {
			case FileInfo.GRAY8 =>  reader.read8bitImage(in).view.map(_&0xff).toArray
			case FileInfo.COLOR8 => reader.read8bitImage(in).view.map(_&0xff).toArray
			case FileInfo.GRAY16_SIGNED => reader.read16bitImage(in).view.map(_&0xffff).toArray
			case FileInfo.GRAY16_UNSIGNED => reader.read16bitImage(in).view.map(_&0xffff).toArray
			case FileInfo.GRAY32_INT => reader.read32bitImage(in).view.map(java.lang.Float.floatToIntBits(_)).toArray
			case FileInfo.GRAY32_UNSIGNED => reader.read32bitImage(in).view.map(java.lang.Float.floatToIntBits(_)).toArray
			case FileInfo.GRAY32_FLOAT => reader.read32bitImage(in).view.map(java.lang.Float.floatToIntBits(_)).toArray
			case FileInfo.GRAY64_FLOAT => reader.read64bitImage(in).view.map(java.lang.Float.floatToIntBits(_)).toArray
			case FileInfo.RGB => reader.readChunkyRGB(in)
			case FileInfo.BGR => reader.readChunkyRGB(in)
			case FileInfo.ARGB => reader.readChunkyRGB(in)
			case FileInfo.ABGR => reader.readChunkyRGB(in)
			case FileInfo.BARG => reader.readChunkyRGB(in)
			case FileInfo.RGB_PLANAR => reader.readPlanarRGB(in)
			case FileInfo.BITMAP => reader.read1bitImage(in).view.map(_&0xff).toArray
			case FileInfo.GRAY12_UNSIGNED => reader.read12bitImage(in).view.map(_&0xffff).toArray
			case FileInfo.GRAY24_UNSIGNED => reader.read24bitImage(in).view.map(java.lang.Float.floatToIntBits(_)).toArray
		})
		in.close
		new TiffStack(stack)
	}

	def loadND2(file: File) = {
    val reader = new ImageProcessorReader(new ChannelSeparator(LociPrefs.makeImageReader))
    reader.setId(file.getAbsolutePath)
    val numFrames = reader.getImageCount
    val width = reader.getSizeX
    val height = reader.getSizeY
    val bitDepth = reader.getBitsPerPixel
    val mask = if(bitDepth == 8) 0xff else 0xffff
    val out = for(i <- 0 until numFrames) yield reader.openProcessors(i)(0).getPixels match {
    	case ba:Array[Byte] => new NumericImage[Int](width, height, ba.view.map(_&0xff).toArray)
    	case sa:Array[Short] => new NumericImage[Int](width, height, sa.view.map(_&0xffff).toArray)
    }
    new TiffStack(out)
  }

}