package shalene

import java.util.ResourceBundle
import java.net.URL
import javafx.event.ActionEvent
import javafx.fxml.{FXML, Initializable}
import javafx.scene.chart.{NumberAxis, LineChart, XYChart}
import javafx.scene.control


class PlotIntensityController extends Initializable {

	@FXML var lineChart : LineChart[Number, Number] = null
	var intensitySeries : XYChart.Series[Number, Number] = null

	def initialize(arg0 : URL, arg1 : ResourceBundle) {
		println(this.getClass.getSimpleName + ".initialize")
	}

	def transferIntensitySeries(intensitySeries : XYChart.Series[Number, Number]) {
		this.intensitySeries = intensitySeries
		lineChart.getData.add(intensitySeries)
	}

}
