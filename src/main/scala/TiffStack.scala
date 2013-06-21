package shalene

import javafx.scene.image.{Image => JfxImg}

/** A stack of normalized Tiff frame objects.
	* @author James R. Thompson, D.Phil
	* @since 11/16/12
	* @constructor An `IndexedSeq[NumericImage[T]]` representing each individual frame.
	*/
class TiffStack[T : Numeric : Manifest](val stack: IndexedSeq[NumericImage[T]]) {
	require(stack.length >= 1, println("TiffStack must have at least 1 frame!"))

	val width = stack(0).width
	val height = stack(0).height

	def getWidth = width
	def getHeight = height

	/** @param The index of the required frame. 
	 	* @return A Tiff object for a given frame */
	def getFrame(index: Int) : NumericImage[T] = {
		require(index < getNumFrames, println("This index is out of bounds! Stack isn't this big. Watch out for 0-indexing, not 1-indexing!"))
		stack(index)
	}

	def getNumFrames : Int = stack.length

	// def getNormalizedStack = {
	// 	val minIntensity = stack.view.map(_.min).min
	// 	val maxIntensity = stack.view.map(_.max).max
	// 	println(s"before normalization min = $minIntensity, max = $maxIntensity")
	// 	val normedImages = stack.view.map(_.normalizedImage(minIntensity, maxIntensity)).force.toIndexedSeq
	// 	println(s"after normalization min = ${normedImages.map(_.min).min}, max = ${normedImages.map(_.max).max}")
	// 	new TiffStack[Double](normedImages)
	// }

	def getJFXFrame(index: Int) = stack(index).getJFXImg

	override def toString : String = width.toString + " x " + height.toString + ", number of frames = " + getNumFrames.toString
}
