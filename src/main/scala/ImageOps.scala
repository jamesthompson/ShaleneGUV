package shalene

object ImageOps {

	def blur[BitDepth](sigma: Double, img: NumericImage[BitDepth]) = (new GrayscaleGaussConvolution(sigma, DefaultSquareKernel)).convolve(img.normalizedImage)

  def contrastAdjust[BitDepth](min: Double, max: Double, img: NumericImage[BitDepth]) : NumericImage[Double] = {
    val buff = img.normalizedImage.getBuffer
    for(i <- 0 until img.getBuffer.length) {
      if(buff(i) < min) buff(i) = min else if(buff(i) < max) buff(i) = max
    }
    (new NumericImage[Double](img.width, img.height, buff)).normalizedImage
  }

}