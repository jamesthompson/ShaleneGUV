package shalene

import java.awt.image.BufferedImage

/** Generic 2D image with any BitDepth pixels
*   Hacked from com.stephentu's Github Canny algorithms.
*
*/
class GenericImage[BitDepth : Manifest](val width: Int, val height: Int, private final val buffer: Array[BitDepth]) extends GridTraversal {
  require(width >= 0, "width must be >= 0")
  require(height >= 0, "height must be >= 0")

  def this(width: Int, height: Int) = this(width, height, new Array[BitDepth](width * height))
  
  final def get(x: Int, y: Int): BitDepth = buffer(y * width + x)

  final def set(x: Int, y: Int, value: BitDepth): Unit = buffer(y * width + x) = value

  def getBuffer = buffer

  def combine[ThatBitDepth, ResBitDepth: Manifest](that: GenericImage[ThatBitDepth])(f: (BitDepth, ThatBitDepth) => ResBitDepth) : GenericImage[ResBitDepth] = {
    // TODO: relax assumption
    assert(width == that.width && height == that.height)
    val newImg = new GenericImage[ResBitDepth](width, height)
    traverseGrid(width, height)((i, j) => newImg.set(i, j, f(get(i, j), that.get(i, j))))
    newImg
  }

  def map[ToBitDepth: Manifest](f: BitDepth => ToBitDepth): GenericImage[ToBitDepth] = mapWithIndex((_, _, e) => f(e))

  def mapWithIndex[ToBitDepth: Manifest](f: (Int, Int, BitDepth) => ToBitDepth): GenericImage[ToBitDepth] = {
    val newImg = new GenericImage[ToBitDepth](width, height)
    traverseGrid(width, height)((i, j) => newImg.set(i, j, f(i, j, get(i, j))))
    newImg
  }

  def max[B >: BitDepth](implicit cmp: Ordering[B]): BitDepth = {
    if (isEmpty) 
      throw new UnsupportedOperationException("Image is empty")
    foldNatural(get(0, 0))((maxSoFar, elem) => if (cmp.gt(elem, maxSoFar)) elem else maxSoFar)
  }

  def min[B >: BitDepth](implicit cmp: Ordering[B]): BitDepth = {
    if (isEmpty) 
      throw new UnsupportedOperationException("Image is empty")
    foldNatural(get(0, 0))((minSoFar, elem) => if (cmp.lt(elem, minSoFar)) elem else minSoFar)
  }

  /** Fold, using the ordering of row scanning (same order as foreach) */
  def foldNatural[B](init: B)(f: (B, BitDepth) => B): B = {
    var acc = init
    foreach(elem => acc = f(acc, elem))
    acc
  }

  def isEmpty: Boolean = width == 0 || height == 0

  def foreachWithIndex[A](f: (Int, Int, BitDepth) => A): Unit = 
    traverseGrid(width, height)((i, j) => f(i, j, get(i, j)))

  def foreach[A](f: BitDepth => A): Unit = foreachWithIndex((_, _, e) => f(e))

  def to2DArray[SuperBitDepth >: BitDepth : Manifest]: Array[Array[SuperBitDepth]] = 
    (0 until height).map(i => buffer.slice(i * width, (i + 1) * width).map(_.asInstanceOf[SuperBitDepth]).toArray).toArray

  def toArray[SuperBitDepth >: BitDepth : Manifest]: Array[SuperBitDepth] = buffer.map(_.asInstanceOf[SuperBitDepth]).toArray

  def histogram[SuperBitDepth >: BitDepth : Manifest](implicit hnum: Numeric[SuperBitDepth]) : Histogram[SuperBitDepth] = new Histogram[SuperBitDepth](toArray)

  /**
   * Returns a slice of this image from (xUpper, yUpper) to (xLower, yLower)
   * (inclusive)
   */
  def subregion[SuperBitDepth >: BitDepth: Manifest](xUpper: Int, yUpper: Int, xLower: Int, yLower: Int)(implicit sbd: Numeric[SuperBitDepth]): GenericImage[SuperBitDepth] = {

    @inline def isOOBX(x: Int) = x < 0 || x >= width
    @inline def isOOBY(y: Int) = y < 0 || y >= height

    require(!isOOBX(xUpper) && !isOOBX(xLower))
    require(!isOOBY(yUpper) && !isOOBY(yLower))
    require(xUpper <= xLower && yUpper <= yLower)

    val subWidth = xLower - xUpper
    val subHeight = yLower - yUpper
    val img = new GenericImage[SuperBitDepth](subWidth, subHeight)
    traverseGrid(xUpper, yUpper, xLower+1, yLower+1)((i, j) => img.set(i, j, get(i, j)))
    img
  }

  def count(p: BitDepth => Boolean): Int = 
    countWithIndex((_, _, e) => p(e))

  def countWithIndex(p: (Int, Int, BitDepth) => Boolean): Int = {
    var cnt = 0
    foreachWithIndex((x, y, e) => if (p(x, y, e)) cnt += 1)
    cnt
  }

  override def toString : String = width.toString + " x " + height.toString + " px, of type : "

}