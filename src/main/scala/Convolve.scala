package shalene

import scala.reflect.ClassTag

/**
*
* Convolutions Code
*
*/

trait VectorOps[V[N], N] {
  def unit: V[N]
  def fromImage(x: Int, y: Int, img: NumericImage[N]): V[N]
  def scale(elem: V[N], factor: N): V[N]
  def add(lhs: V[N], rhs: V[N]): V[N] 
}

object VectorOps { implicit def singlePxOp[N : Numeric] = new SinglePixelOps[N] }

abstract class PixelVectorOps[P[N] <: Pixel[P, N], N] extends VectorOps[P, N] {
  def scale(elem: P[N], factor: N) = elem.scale(factor)
  def add(lhs: P[N], rhs: P[N]) = lhs + rhs
}

class SinglePixelOps[N](implicit ev: Numeric[N]) extends PixelVectorOps[SinglePixel, N] {
  def unit = new SinglePixel[N](ev.zero) 
  def fromImage(x: Int, y: Int, img: NumericImage[N]) = new SinglePixel[N](img.get(x, y))
}


class SquareMask[N](side: Int, data: Array[N])(implicit ev: Numeric[N]) extends GridTraversal {
  private val mid = side / 2
  def getData(x: Int, y: Int): N = data(y * side + x)
  def evaluate[V[N]](x: Int, y: Int, img: NumericImage[N])(implicit ops: VectorOps[V, N]): V[N] = {
    @inline def outOfXBounds(pos: Int) = pos < 0 || pos >= img.width
    @inline def outOfYBounds(pos: Int) = pos < 0 || pos >= img.height
    val pxes = mapGrid(side, side)((i, j) => {
      val curX = i - mid + x
      val curY = j - mid + y
      if (outOfXBounds(curX) || outOfYBounds(curY)) ops.unit 
      else ops.scale(ops.fromImage(curX, curY, img), getData(i, j))
    })
    pxes.foldLeft(ops.unit) { case (acc, vec) => ops.add(acc, vec) }
  }
}

case class SquareKernel(side: Int) extends GridTraversal {
  def computeMask[N](f: (Int, Int) => N)(implicit ev: Numeric[N], cm: ClassTag[N]): SquareMask[N] = {
    val data = mapGrid(side, side)(f) 
    new SquareMask[N](side, data.toArray) 
  }
}

object DefaultSobelKernel extends SquareKernel(3)
object DefaultSquareKernel extends SquareKernel(5)

// Main convolution trait
trait Convolution[V[N], N, Result] extends GridTraversal {
  val kernel: SquareKernel
  protected def newMask: SquareMask[N]
  protected def newResult(img: NumericImage[N]): Result
  protected def update(x: Int, y: Int, agg: V[N], canvas: Result): Unit
  protected def widthOf(canvas: Result): Int
  protected def heightOf(canvas: Result): Int
  def apply(img: NumericImage[N]): Result = convolve(img)
  protected implicit def ops: VectorOps[V, N]
  def convolve(img: NumericImage[N]): Result = {
    val canvas = newResult(img)
    val mask   = newMask
    traverseGrid(widthOf(canvas), heightOf(canvas))((x, y) => update(x, y, mask.evaluate[V](x, y, img), canvas))
    canvas
  }
}

// Main Gaussian style convolution
trait GaussConvolution[Px[Double] <: Pixel[Px, Double]] extends Convolution[Px, Double, NumericImage[Double]] {
  val sigma: Double
  private def gauss(x: Int, y: Int): Double = {
    val sigma_2 = sigma * sigma
    1.0 / (2.0 * math.Pi * sigma_2) * math.exp( - (x*x + y*y).toDouble / (2.0 * sigma_2) ) 
  }
  def newMask = {
    val mid = kernel.side / 2
    kernel.computeMask((x,y) => gauss(math.abs(x - mid), math.abs(y - mid)))
  }
  def widthOf(img: NumericImage[Double]) = img.width
  def heightOf(img: NumericImage[Double]) = img.height
}

// Implementation of grayscale gaussian convolution
class GrayscaleGaussConvolution(val sigma: Double, val kernel: SquareKernel) extends GaussConvolution[SinglePixel] {
  def ops = implicitly[VectorOps[SinglePixel, Double]]
  def newResult(img: NumericImage[Double]) = new NumericImage(img.width, img.height)
  def update(x: Int, y: Int, agg: SinglePixel[Double], img: NumericImage[Double]) = img.set(x, y, agg.value.toDouble)
}

// Main Sobel style convolution
trait SobelOperator extends Convolution[SinglePixel, Double, GenericImage[Double]] {
  def ops = implicitly[VectorOps[SinglePixel, Double]]
  def newResult(img: NumericImage[Double]) = new GenericImage[Double](img.width, img.height)
  def update(x: Int, y: Int, agg: SinglePixel[Double], img: GenericImage[Double]) = img.set(x, y, agg.value.toDouble)
  def widthOf(img: GenericImage[Double]) = img.width
  def heightOf(img: GenericImage[Double]) = img.height
}

// Implementation of x-direction Sobel gradient convolution
object SobelOperatorX extends SobelOperator {
  private val sobelMaskXData = 
    Array(-1.0, 0.0, 1.0,
          -2.0, 0.0, 2.0,
          -1.0, 0.0, 1.0)
  object sobelMaskX extends SquareMask[Double](3, sobelMaskXData) 
  val kernel = DefaultSobelKernel
  val newMask = sobelMaskX
}

// Implementation of y-direction Sobel gradient convolution
object SobelOperatorY extends SobelOperator {
  private val sobelMaskYData = 
    Array(-1.0, -2.0, -1.0,
           0.0,  0.0,  0.0,
           1.0,  2.0,  1.0)
  object sobelMaskY extends SquareMask[Double](3, sobelMaskYData) 
  val kernel = DefaultSobelKernel
  val newMask = sobelMaskY
}
