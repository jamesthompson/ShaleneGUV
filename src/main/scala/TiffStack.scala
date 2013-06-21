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

	def getJFXFrame(index: Int) = stack(index).getJFXImg

	override def toString : String = width.toString + " x " + height.toString + ", number of frames = " + getNumFrames.toString
}
