package com.mycompany.emojimaker;

import Classes.Usuario;
import TDASimplement.ArrayList;
import javafx.application.Application;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.stage.Stage;

import java.io.IOException;

/**
 * JavaFX App
 */
public class App extends Application {
    public static ArrayList<Usuario> usuarios=new ArrayList<>();
    private static Scene scene;
    public static Usuario usuarioSeleccionado=null;

    @Override
    public void start(Stage stage) throws IOException {
        Usuario uprueba=new Usuario("dtruiz", "dtruiz");
        usuarios.addLast(uprueba);
        scene = new Scene(loadFXML("welcomeWindow"), 700, 600);
        stage.setScene(scene);
        stage.show();
    }

    static void setRoot(String fxml) throws IOException {
        scene.setRoot(loadFXML(fxml));
    }

    private static Parent loadFXML(String fxml) throws IOException {
        FXMLLoader fxmlLoader = new FXMLLoader(App.class.getResource(fxml + ".fxml"));
        return fxmlLoader.load();
    }

    public static void main(String[] args) {
        launch();
    }

}