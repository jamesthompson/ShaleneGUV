package shalene;

import java.net.URL;
import java.util.logging.Level;
import java.util.logging.Logger;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Scene;
import javafx.scene.layout.AnchorPane;
import javafx.stage.Stage;
import java.net.URLClassLoader;
import java.lang.reflect.Method;
import java.io.IOException;

/**
 * GUV Analyzer Application v.1.0
 * @author James R. Thompson
 */

public class Launch extends Application {

    public static void main(String[] args) {
        Application.launch(Launch.class, args);
    }

    @Override
    public void start(Stage stage) throws Exception {
        try {
            FXMLFactory.loadFXMLClass("/AnalyzerStageMainSCALA.fxml", "Shalene's GUV Analyzer!");
        } catch (Exception ex) {
            Logger.getLogger(Launch.class.getName()).log(Level.SEVERE, null, ex);
        }
    }

}