package shalene


class NumericImage[@specialized BitDepth: Numeric : Manifest](val width: Int, val height: Int, private final val buffer: Array[BitDepth]) extends GridTraversal {
	require(width > 0 && height > 0, "dimensions must be > 0 px")

	//Automagical Numeric implicit object!
  val ev = implicitly[Numeric[BitDepth]]

  implicit def arrayLocFromCart(in: (Int, Int)) : Int = in._2 * width + in._1
  implicit def cartLocFromArrayIndex(in: Int) : (Int, Int) = (in % width, math.floor(in / width).toInt)

  def this(width: Int, height: Int) = this(width, height, new Array[BitDepth](width * height))

  def getJFXImg = ImageVisualizer.getJavaFXImage(this.normalizedImage, width, height)

  // def getEdge(min: Double, max: Double) = {
  //   val imageEdge : NumericImage[Double] = ImageOps.detectEdge(min, max, normalizedImage)
  //   ImageVisualizer.getJavaFXImage(imageEdge, width, height)
  // }

  def getBuffer = buffer

  final def get(x: Int, y: Int) : BitDepth = buffer(y * width + x)

  final def set(x: Int, y: Int, value: BitDepth) : Unit = buffer(y * width + x) = value

  def min = buffer.min
  def max = buffer.max

  def mapCombine[ThatBitDepth: Numeric: Manifest, ResultBitDepth: Numeric: Manifest]
  (that: NumericImage[ThatBitDepth])(f: (BitDepth, ThatBitDepth) => ResultBitDepth) : NumericImage[ResultBitDepth] = {
    assert(width == that.width && height == that.height)
    val newImg = new NumericImage[ResultBitDepth](width, height)
    traverseGrid(width, height)((i, j) => newImg.set(i, j, f(get(i, j), that.get(i, j))))
    newImg
  }

  def map[ResultBitDepth: Numeric: Manifest](f: BitDepth => ResultBitDepth) : NumericImage[ResultBitDepth] = mapWithIndex((_, _, e) => f(e))

  def mapWithIndex[ResultBitDepth: Numeric: Manifest](f: (Int, Int, BitDepth) => ResultBitDepth) : NumericImage[ResultBitDepth] = {
    val newImg = new NumericImage[ResultBitDepth](width, height)
    traverseGrid(width, height)((i, j) => newImg.set(i, j, f(i, j, get(i, j))))
    newImg
  }

  def to2DArray[ResultBitDepth >: BitDepth: Numeric : Manifest] : Array[Array[ResultBitDepth]] = 
    (0 until height).par.map(i => buffer.slice(i * width, (i + 1) * width).par.map(_.asInstanceOf[ResultBitDepth]).toArray).toArray

  // Automagically gets a subregion of this image and returns the same bitdepth. n.b. first 2 params should be bigger than last 2.
  def subregion(xUpper: Int, yUpper: Int, xLower: Int, yLower: Int) : NumericImage[BitDepth] = {
    @inline def isOOBX(x: Int) = x < 0 || x >= width
    @inline def isOOBY(y: Int) = y < 0 || y >= height
    require(!isOOBX(xUpper) && !isOOBX(xLower))
    require(!isOOBY(yUpper) && !isOOBY(yLower))
    require(xUpper <= xLower && yUpper <= yLower)
    val subWidth = xLower - xUpper
    val subHeight = yLower - yUpper
    val img = new NumericImage[BitDepth](subWidth, subHeight)
    traverseGrid(xUpper, yUpper, xLower, yLower)((i, j) => img.set(i - xUpper, j - yUpper, get(i, j)))
    img
  }

  // Normalize from min and max to 0 and 255 in ImageVisualizer
  def normalizedPixels: Array[Double] = {
    val doubled = buffer.view.map(ev.toDouble(_)).toArray
    val mind = doubled.min
    val maxd = doubled.max
    doubled.map(v => (v - mind) * (255 / (maxd - mind)))
  }

  def getDoubleImage : NumericImage[Double] = new NumericImage[Double](width, height, buffer.view.map(ev.toDouble(_)).toArray)

  // Normalize between 0 and 1 and return a new NumericImage object
  def normalizedImage : NumericImage[Double] = new NumericImage(width, height, normalizedPixels)

  // def normalizedImage(mind: BitDepth, maxd: BitDepth) : NumericImage[Double] = new NumericImage(width, height, normalizedPixels(ev.toDouble(mind), ev.toDouble(maxd)))

}