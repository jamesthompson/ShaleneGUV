package shalene

import java.io._
import java.math.BigDecimal
import java.net.URL
import java.text.NumberFormat
import java.util.{List, ResourceBundle}
import java.util.logging.Level
import java.util.logging.Logger
import javafx.beans.value.{ChangeListener, ObservableValue}
import javafx.collections.{FXCollections, ObservableList}
import javafx.concurrent.{Service, Task, Worker}
import javafx.event.{ActionEvent, Event}
import javafx.fxml.{FXML, FXMLLoader, Initializable, JavaFXBuilderFactory}
import javafx.application.Platform
import javafx.scene.Scene
import javafx.scene.chart.{NumberAxis, LineChart, XYChart}
import javafx.scene.control._
import javafx.scene.input.{Clipboard, ClipboardContent}
import javafx.scene.layout.{AnchorPane, GridPane, VBox}
import javafx.stage.{FileChooser, Stage}
import jfxtras.labs.scene.control.BigDecimalField
import scala.collection.JavaConversions._

/**
 * Main app controller class
 * @author James R. Thompson, D.Phil
 * @since Jun 19, 2013
 * Malmstadt Lab - Mork Family Dept. Chem. Eng. & Mat. Sci. - University of Southern California
 */

class Controller extends Initializable {

	// Fields

	val datasets : ObservableList[GUV] = FXCollections.observableArrayList()

	var rsrcs : ResourceBundle = null
	var series : XYChart.Series[Number, Number] = null
	var devSeries : XYChart.Series[Number, Number] = null
	var presentFrame : Int = 0
	var personField : TextField = null
	var dateField : TextField = null
	var averageRadiusField : TextField = null
	var pixelField : TextField = null
	var timeField : TextField = null
	var root : GridPane = null
	var modePicker : BigDecimalField = null
	var kPicker : BigDecimalField = null
	var sPicker : BigDecimalField = null
	@FXML var loadImageMenuItem : MenuItem = null
	@FXML var listView : ListView[GUV] = null
	@FXML var frameSlider : Slider = null
	@FXML var progressBar : ProgressBar = null
	@FXML var lineChart : LineChart[Number, Number] = null
	@FXML var deviationChart : LineChart[Number, Number] = null
	@FXML var mainAnchorPane : AnchorPane = null
	@FXML var rightBox : VBox = null
	@FXML var legMenu : CustomMenuItem = null
	@FXML var simulMenu : CustomMenuItem = null
	@FXML var runLegButton : Button = null
	@FXML var simulButton : Button = null
	@FXML var XAxis : NumberAxis = null
	@FXML var YAxis : NumberAxis = null

	// ----| Initialization

	def initialize(arg0:URL, arg1:ResourceBundle) {
		this.rsrcs = arg1
		println(this.getClass.getSimpleName + ".initialize")
		listView.setItems(datasets)
		listView.getSelectionModel.setSelectionMode(SelectionMode.SINGLE)
		frameSlider.valueProperty.addListener(new ChangeListener[Number] {
			def changed(arg0 : ObservableValue[_ <: Number], arg1 : Number, arg2 : Number) {
				if (lineChart != null && datasets.size > 0) {
					updateGraph(arg2.intValue)
				}
			}
		})
	}

	// ----| Event Handling

	def exitApp(event : ActionEvent) = Platform.exit

	def vesicleSelectionChanged(event : Event) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset and get harmonics first!")
			case false => {
				lineChart.getData.remove(series)
				deviationChart.getData.remove(devSeries)
				series = createSeries
				devSeries = createDevSeries
				lineChart.getData.add(series)
				deviationChart.getData.add(devSeries)
				frameSlider.setValue(0)
			}
		}
	}

	def sortContours(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				listView.getSelectionModel.getSelectedItem.checkContoursOk
			}
		}
	}

	def deleteContour(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				val index = frameSlider.getValue.toInt
				val originalLength = listView.getSelectionModel.getSelectedItem.contours.length
				listView.getSelectionModel.getSelectedItem.killContour(index)
				frameSlider.setMax(frameSlider.getMax - 1)
				println("Deleted frame number " + index + ", was " + originalLength + " frames, now " + listView.getSelectionModel.getSelectedItem.contours.length + " long" )
			}
		}
	}

	def skipForward(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				val index = frameSlider.getValue.toInt
				if(index < frameSlider.getMax) frameSlider.setValue(index + 1) else frameSlider.setValue(0)
			}
		}
	}

	def skipBackward(event: ActionEvent) {
		listView.getSelectionModel.isEmpty match {
			case true => warn("Please load a dataset first!")
			case false => {
				val index = frameSlider.getValue.toInt
				if(index > 0) frameSlider.setValue(index - 1) else frameSlider.setValue(frameSlider.getMax)
			}
		}
	}

	def createSeries : XYChart.Series[Number, Number] = {
		val guv = listView.getSelectionModel.getSelectedItem
		frameSlider.setMax(guv.contours.size - 1)
		val scale = guv.calcScale
		XAxis.setLowerBound((-scale * 1.1).toInt - 1)
		XAxis.setUpperBound((scale * 1.1).toInt + 1)
		YAxis.setLowerBound((-scale * 1.1).toInt - 1)
		YAxis.setUpperBound((scale * 1.1).toInt + 1)
		guv.getFrameChart(0)
	}

	def createDevSeries : XYChart.Series[Number, Number] = listView.getSelectionModel.getSelectedItem.getFrameDeviationChart(0)

	def updateGraph(frameNumber : Int) {
		val guv = listView.getSelectionModel.getSelectedItem
		lineChart.getData.remove(series)
		deviationChart.getData.remove(devSeries)
		presentFrame = frameNumber
		series = guv.getFrameChart(frameNumber)
		devSeries = guv.getFrameDeviationChart(frameNumber)
		lineChart.getData.add(series)
		deviationChart.getData.add(devSeries)
	}

	def copyCartesianPoints(event : ActionEvent) {
		println("Cartesian Points on Clipboard")
		val clipboard = Clipboard.getSystemClipboard
		val content = new ClipboardContent
		if (listView.getSelectionModel.isEmpty == false) {
			content.putString(listView.getSelectionModel.getSelectedItem.getContour(presentFrame).toCartString)
			clipboard.setContent(content)
		}
	}

	// ----| File Handling

	def saveGUV(event : ActionEvent) {
		val fc = new FileChooser
		fc.setInitialDirectory(new File("/Users/James/Desktop/"))
		val file = fc.showSaveDialog(mainAnchorPane.sceneProperty.get.getWindow)
		val task = new Task[Void] {
			protected def call : Void = {
				val guv = listView.getSelectionModel.getSelectedItem
				try {
					val fos = new FileOutputStream(file.getPath + guv.toString + ".guv")
					val out = new ObjectOutputStream(fos)
					out.writeObject(guv)
					out.close
				}
				catch {
					case ex : IOException => {
					}
				}
				null
			}
		}
		val loadThread = new Thread(task)
		loadThread.start
	}

	def saveAllGUVs(event : ActionEvent) {
		val fc = new FileChooser
		val file = fc.showSaveDialog(mainAnchorPane.sceneProperty.get.getWindow)
		val task = new Task[Void] {
			protected def call = {
				for(guv <- datasets) {
					try {
						val fos = new FileOutputStream(file.getPath + guv.toString + ".guv")
						val out = new ObjectOutputStream(fos)
						out.writeObject(guv)
						out.close
					}
					catch {
						case ex : IOException => {
						}
					}
				}
				null
			}
		}
		val loadThread = new Thread(task)
		loadThread.start
	}

	def loadGUVs(event : ActionEvent) {
		val fc = new FileChooser
		val extension = new FileChooser.ExtensionFilter("GUV files (*.guv)", "*.guv")
		fc.getExtensionFilters.add(extension)
		fc.setInitialDirectory(new File("/Users/James/Desktop/"))
		val list = fc.showOpenMultipleDialog(mainAnchorPane.sceneProperty.get.getWindow)
		val task = new Task[Void] {
			protected def call : Void = {
				for(file <- list) {
					var guv : GUV = null // Horrible mutable state! Will figure out how to fix later!! I promise!
					try {
						val fis = new FileInputStream(file)
						val in = new ObjectInputStream(fis)
						guv = in.readObject.asInstanceOf[GUV]
						in.close
					}
					catch {
						case ex : IOException => {
						}
					}
					datasets.add(guv)
				}
				null
			}
		}
		val loadThread = new Thread(task)
		loadThread.start
	}

	def deleteGUV(event : ActionEvent) {
		datasets.isEmpty match {
			case true => ()
			case false => {
				datasets.remove(listView.getSelectionModel.getSelectedItem)
				lineChart.getData.remove(series)
				deviationChart.getData.remove(devSeries)
			}
		}		
	}

	def plotIntensity(event: ActionEvent) {
		try {
			val intensityLoad : PlotIntensityController = FXMLFactory.loadFXMLClass("/AvgIntensityStage.fxml", "Average Intensity vs. Time").asInstanceOf[PlotIntensityController]
			val guv = listView.getSelectionModel.getSelectedItem
			intensityLoad.transferIntensitySeries(guv.getAvgIntensitySeries)
		}
		catch {
			case ex : Exception => {
				Logger.getLogger(classOf[Launch].getName).log(Level.SEVERE, null, ex)
			}
		}
	}

	// ----| Image Importer Loader

	def launchImageLoad(event : ActionEvent) {
		try {
			val imLoad : ImageLoadController = FXMLFactory.loadFXMLClass("/ImageLoadStage.fxml", "Image Loader").asInstanceOf[ImageLoadController]
			imLoad.transferGUVList(datasets)
		}
		catch {
			case ex : Exception => {
				Logger.getLogger(classOf[Launch].getName).log(Level.SEVERE, null, ex)
			}
		}
	}

	
	// ----| Warning Dialog Loader

	def warn(warning : String) {
		val loader : FXMLLoader = new FXMLLoader
		val in : InputStream = classOf[Launch].getResourceAsStream("/WarningDialog.fxml")
		loader.setBuilderFactory(new JavaFXBuilderFactory)
		loader.setLocation(classOf[Launch].getResource("/WarningDialog.fxml"))
		var page : AnchorPane = null
		try {
			page = loader.load(in).asInstanceOf[AnchorPane]
		}
		finally {
			in.close
		}
		val scene : Scene = new Scene(page)
		val stage : Stage = new Stage
		stage.setScene(scene)
		stage.sizeToScene
		stage.setTitle("Warning!")
		stage.show
		try {
			val dialog : DialogController = loader.getController.asInstanceOf[DialogController]
			dialog.set(stage, warning)
		}
		catch {
			case ex : Exception => {
				Logger.getLogger(classOf[Launch].getName).log(Level.SEVERE, null, ex)
			}
		}
	}

}