package shalene

import java.io.File
import java.net.URL
import java.util.ResourceBundle
import javafx.animation.FadeTransition
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.ObservableList
import javafx.concurrent.{Service, Task, Worker}
import javafx.event.ActionEvent
import javafx.fxml.{FXML, Initializable}
import javafx.geometry.Orientation
import javafx.scene.control._
import javafx.scene.image.ImageView
import javafx.scene.layout.{AnchorPane, VBox}
import javafx.scene.Group
import javafx.scene.paint.Color
import javafx.scene.shape._
import javafx.stage.FileChooser
import javafx.util.Duration
import jfxtras.labs.scene.control.gauge._
import javafx.scene.control
import scala.math._
import javafx.scene.shape.Path

/**
* Image loading controller
* @author James R. Thompson, D.Phil
* @since Jun 19, 2013
* Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
*/

class ImageLoadController extends Initializable {

	// Fields

	var file : File = null
	var tiffStack : TiffStack[_] = null
	var readyft : FadeTransition = null
	var guvList : ObservableList[GUV] = null
	var edgeGroup : Group = new Group
	@FXML var imageLoadAnchorPane : AnchorPane = null
	@FXML var progressBar : ProgressBar = null
	@FXML var imagePreview : ImageView = null
	@FXML var ckfPreview : ImageView = null
	@FXML var frameSlider : Slider = null
	@FXML var edgePreviewButton : CheckBox = null
	@FXML var chooseButton : Button = null
	@FXML var readyButton : Button = null
	@FXML var controllerBox : VBox = null
	@FXML var imageBox : VBox = null
	@FXML var toolBar : ToolBar = null

	var anglesLCD : Lcd = null
	var thresholdLCD : Lcd = null
	var anglesSlider : Slider = null
	var thresholdSlider : Slider = null
	var radiusLCD : Lcd = null
	var radiusSlider : Slider = null

	// Functions

	def initialize(arg0 : URL, arg1 : ResourceBundle) {
		println(this.getClass.getSimpleName + ".initialize")
		makeControllers
		imageLoadAnchorPane.getChildren.add(edgeGroup)
	}

	def transferGUVList(guvList : ObservableList[GUV]) {
		this.guvList = guvList
	}

	def pickFile(event : ActionEvent) {
		val fc : FileChooser = new FileChooser
		val extension : FileChooser.ExtensionFilter = new FileChooser.ExtensionFilter("TIFF files (*.tif)", "*.tif")
		fc.getExtensionFilters.add(extension)
		file = fc.showOpenDialog(imageLoadAnchorPane.sceneProperty.get.getWindow)
		tiffStack = ImageLoad.load(file)
		frameSlider.setValue(0)
		frameSlider.setMax(tiffStack.getNumFrames - 1)
		imagePreview.setImage(tiffStack.getJFXFrame(0))
		visualizeControllers
		toolBar.getItems.remove(chooseButton)
	}

	def updatePreviewImage(frameIndex : Int) = imagePreview.setImage(tiffStack.getJFXFrame(frameIndex))

	def updateEdge(frame:Int) {
		val ef = new EdgeFinder(tiffStack.getFrame(frame).getDoubleImage.getBuffer, tiffStack.getWidth, tiffStack.getHeight)
		val calc = ef.convImgToPolar(anglesSlider.getValue.toInt, thresholdSlider.getValue.toDouble, radiusSlider.getValue.toInt)
		val edgeLocation = calc.map(_._2)
		val pixelData = calc.map(_._1)
		val polarImage = getImgFromEdgeFinder(pixelData).getJFXImg
		val width = pixelData(0).length
		val height = pixelData.length
		ckfPreview.setImage(polarImage)
		updateDrawEdge(edgeLocation, width, height)
		// println(s"Average Intensity = $averageIntensity")
	}

	def edgeIntensity(pl : (List[Double], PolarLocation)) : Double = {
		val index = pl._2.y
		val negIndex = index - 1
		val posIndex = index + 1
		val range = posIndex - negIndex
		val subList = pl._1.slice(negIndex.toInt, posIndex.toInt)
		subList.sum / range // average output
	}

	private def getImgFromEdgeFinder(in: List[List[Double]]) = { 
		val width = in(0).length
		new NumericImage[Double](width, in.length, in.flatten.toArray)
	}

	def updateDrawEdge(location:List[PolarLocation], width:Int, height:Int) {
		edgeGroup.getChildren.removeAll(edgeGroup.getChildren)
		val xscale = ckfPreview.getFitWidth / width
		val yscale = ckfPreview.getFitHeight / height
		val path = new Path
		path.setStroke(Color.RED)
	    path.setStrokeWidth(1.0)
	    path.setOpacity(1.0)
	    path.getElements().add(new MoveTo(imageBox.getLayoutX + (location(0).x + 0.5) * xscale, imageBox.getLayoutY + (location(0).y + 0.5) * yscale))
	    location.filter(location.indexOf(_) != 0).map(p => path.getElements().add(new LineTo(imageBox.getLayoutX + (p.x + 0.5) * xscale, imageBox.getLayoutY + (p.y + 0.5) * yscale)))
		edgeGroup.getChildren.add(path)
	}

	def readied(event : ActionEvent) {
		readyft.stop
		readyButton.setOpacity(1.0)
		readyButton.setText("Importing...")
		doImport
	}


	def doImport {
		println("GUV import kicked off...")
		val guvBuilder : Service[GUV] = new Service[GUV] {
			protected def createTask : Task[GUV] = {
				return new Task[GUV] {
					protected def call : GUV = {
						val guv : GUV = new GUV(file.getAbsolutePath)
						for(img <- tiffStack.stack) {
							val ef = new EdgeFinder(img.getDoubleImage.getBuffer, tiffStack.getWidth, tiffStack.getHeight)
							val calc = ef.convImgToPolar(anglesSlider.getValue.toInt, thresholdSlider.getValue.toDouble, radiusSlider.getValue.toInt)
							val avgInt = calc.map(edgeIntensity).sum / calc.length
							val cont = new Contour(ef.getPoints(calc), avgInt)
							cont.sortPoints
							updateProgress(tiffStack.stack.indexOf(img), tiffStack.getNumFrames - 1)
							guv.addContour(cont)
							guv.saveAvgIntensity
						}
						return guv
					}
				}
			}
		}
		guvBuilder.stateProperty.addListener(new ChangeListener[Worker.State] {
			def changed(observableValue : ObservableValue[_ <: Worker.State], oldState : Worker.State, newState : Worker.State) {
				newState match {
					case Worker.State.SUCCEEDED => println("Finished loading")
					val output : GUV = guvBuilder.valueProperty.getValue
					guvList.add(output)
					progressBar.setVisible(false)
					readyButton.setVisible(false)
					case Worker.State.FAILED => println("Failed")
					case Worker.State.RUNNING => println("Running")
					case Worker.State.CANCELLED => println("Cancelled")
					case Worker.State.READY => println("Ready")
					case Worker.State.SCHEDULED => println("Scheduled")
				}
			}
		})
		progressBar.setProgress(0)
		progressBar.progressProperty.bind(guvBuilder.progressProperty)
		progressBar.setVisible(true)
		guvBuilder.start
	}

	def visualizeControllers {
		val cdft : FadeTransition = new FadeTransition(Duration.millis(1000), controllerBox)
		cdft.setFromValue(0.0)
		cdft.setToValue(1.0)
		cdft.setCycleCount(1)
		cdft.setAutoReverse(false)
		val slideft : FadeTransition = new FadeTransition(Duration.millis(1000), frameSlider)
		slideft.setFromValue(0.0)
		slideft.setToValue(1.0)
		slideft.setCycleCount(1)
		slideft.setAutoReverse(false)
		readyButton.setVisible(true)
		readyft = new FadeTransition(Duration.millis(500), readyButton)
		readyft.setFromValue(0.0)
		readyft.setToValue(1.0)
		readyft.setCycleCount(Integer.MAX_VALUE)
		readyft.setAutoReverse(true)
		cdft.play
		slideft.play
		readyft.play
		edgePreviewButton.setVisible(true)
	}

	def makeControllers = {
		// val StyleRadius : StyleModel = StyleModelBuilder.create.lcdDesign(LcdDesign.DARKBLUE).lcdValueFont(Gauge.LcdFont.LCD).lcdUnitStringVisible(true).lcdDecimals(0).lcdNumberSystemVisible(true).build
		// val StyleAll : StyleModel = StyleModelBuilder.create.lcdDesign(LcdDesign.DARKBLUE).lcdValueFont(Gauge.LcdFont.LCD).lcdUnitStringVisible(true).lcdDecimals(3).lcdNumberSystemVisible(true).build
		// anglesLCD = LcdBuilder.create.styleModel(StyleRadius).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(0).maxMeasuredValueDecimals(0).formerValueVisible(true).title("Number of Angles").unit("arb.").value(360).trendVisible(true).build
		// anglesLCD.setPrefSize(200, 50)
		// anglesLCD.setMaxValue(1000)
		// thresholdLCD = LcdBuilder.create.styleModel(StyleAll).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(3).maxMeasuredValueDecimals(3).formerValueVisible(true).title("Threshold").unit("%").value(10.0).trendVisible(true).build
		// thresholdLCD.setPrefSize(200, 50)
		// thresholdLCD.setMaxValue(250.0)
		// radiusLCD = LcdBuilder.create.styleModel(StyleRadius).minMeasuredValueVisible(true).maxMeasuredValueVisible(true).minMeasuredValueDecimals(0).maxMeasuredValueDecimals(0).formerValueVisible(true).title("Radius Treshold").unit("px").value(0).trendVisible(true).build
		// radiusLCD.setPrefSize(200, 50)
		// radiusLCD.setMaxValue(500)

		anglesSlider = new control.Slider()
		anglesSlider.setMajorTickUnit(10)
		anglesSlider.setMin(0)
		anglesSlider.setMax(1000.0)
		anglesSlider.setValue(360.0)
		anglesSlider.setOrientation(Orientation.HORIZONTAL)
		anglesSlider.setMinorTickCount(0)
		anglesSlider.setShowTickLabels(false)
		anglesSlider.setShowTickMarks(false)
		anglesSlider.setSnapToTicks(true)


		thresholdSlider = new control.Slider()
		thresholdSlider.setMajorTickUnit(0.1)
		thresholdSlider.setMin(0.0)
		thresholdSlider.setMax(100.0)
		thresholdSlider.setValue(5.0)
		thresholdSlider.setOrientation(Orientation.HORIZONTAL)
		thresholdSlider.setMinorTickCount(0)
		thresholdSlider.setShowTickLabels(false)
		thresholdSlider.setShowTickMarks(false)
		thresholdSlider.setSnapToTicks(true)

		radiusSlider = new control.Slider()
		radiusSlider.setMajorTickUnit(1)
		radiusSlider.setMin(0)
		radiusSlider.setMax(500.0)
		radiusSlider.setValue(0)
		radiusSlider.setOrientation(Orientation.HORIZONTAL)
		radiusSlider.setMinorTickCount(0)
		radiusSlider.setShowTickLabels(false)
		radiusSlider.setShowTickMarks(false)
		radiusSlider.setSnapToTicks(true)

		// controllerBox.getChildren.add(anglesLCD)
		controllerBox.getChildren.add(anglesSlider)
		// controllerBox.getChildren.add(thresholdLCD)
		controllerBox.getChildren.add(thresholdSlider)
		// controllerBox.getChildren.add(radiusLCD)
		controllerBox.getChildren.add(radiusSlider)
		controllerBox.setOpacity(0)
		setSliderParams
	}

	def setSliderParams {
		frameSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (tiffStack != null && edgePreviewButton.isSelected) {
					updatePreviewImage(arg2.intValue)
					updateEdge(arg2.intValue)
				}
				else if (tiffStack != null) {
					updatePreviewImage(arg2.intValue)
				}
			}
		})
		anglesSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (tiffStack != null) {
					// anglesLCD.setValue(arg2.doubleValue)
					updateEdge(arg2.intValue)
				}
			}
		})
		thresholdSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (tiffStack != null) {
					// thresholdLCD.setValue(arg1.doubleValue)
					updateEdge(arg2.intValue)
				}
			}
		})
		radiusSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (tiffStack != null && edgePreviewButton.isSelected) {
					// radiusLCD.setValue(arg2.intValue)
					updateEdge(arg2.intValue)
				}
			}
		})
	}
}