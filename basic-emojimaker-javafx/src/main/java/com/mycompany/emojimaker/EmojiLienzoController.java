/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/javafx/FXMLController.java to edit this template
 */
package com.mycompany.emojimaker;

import Classes.Emoji;
import Classes.Proyecto;
import javafx.scene.image.Image;

import TDASimplement.ArrayList;

import TDASimplement.DCLList;
import TDASimplement.NodeDCLL;
import java.awt.Rectangle;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.Group;
import javafx.scene.SnapshotParameters;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.image.WritableImage;
import javafx.scene.layout.AnchorPane;
import javafx.scene.layout.BorderPane;
import javafx.scene.layout.HBox;
import javafx.scene.layout.VBox;
import javax.imageio.ImageIO;
import java.awt.image.BufferedImage;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import javafx.collections.FXCollections;
import javafx.collections.ObservableArray;
import javafx.collections.ObservableList;
import javafx.embed.swing.SwingFXUtils;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Alert;
import javafx.scene.control.ComboBox;
import javafx.scene.control.ScrollPane;

import javafx.scene.effect.Effect;

import javafx.scene.control.TextInputDialog;

import javafx.scene.layout.FlowPane;
import javafx.stage.FileChooser;
import javafx.stage.FileChooser.ExtensionFilter;
import javafx.stage.Stage;
import javax.swing.JFileChooser;
import javax.swing.filechooser.FileNameExtensionFilter;
import pruebas.Main;
import static pruebas.Main.cargar;

/**
 * FXML Controller class
 *
 * @author USUARIO
 */

public class EmojiLienzoController implements Initializable {
    @FXML 
    
    private HBox conBtn_Menu;
    @FXML
    private Button btnPrev;
    @FXML
    private Button btnNext;
    @FXML
    private ComboBox comboBoxOpciones;
    @FXML
    private ScrollPane scrollPaneFeatures;
    @FXML
    private VBox contenedorBotones;
    @FXML
    private Button btnGuardar;
    @FXML
    private VBox lienzo;
    @FXML
    private AnchorPane emojiBlock;
    @FXML
    private ImageView emojiFace;
    @FXML
    private ImageView emojiBrows;
    @FXML
    private ImageView emojiEyes;
    @FXML
    private ImageView emojiMouth;
    @FXML
    private VBox featuresListContainers;
    @FXML
    private AnchorPane panelPrincipal;
    @FXML
    private HBox contenedorBotonesYLienzo;
    @FXML
    private Button btnExportar;
    @FXML
    private Button deleteFeatButton;
    @FXML
    private FlowPane contenedorScroll;
     @FXML
    private Button newFtButton;
    private DCLList<ImageView> ojos=new DCLList<>();
    private DCLList<ImageView> bocas=new DCLList<>();
    private DCLList<ImageView> caras=new DCLList<>();
    private DCLList<ImageView> cejas=new DCLList<>();
    private DCLList<ImageView> extras=new DCLList<>();
    private NodeDCLL<ImageView> nodoF;
    private ImageView ivSelected;
    private DCLList<ImageView> selectedList;
    
    ObservableList<String> options = FXCollections.observableArrayList(
                "ojos",
                "cara",
                "ceja",
                "accesorios",
                "boca"
            
        );
    
    @Override
    public void initialize(URL url, ResourceBundle rb) {
        llenarLists();
        

        //Llena opciones y evento segun seleccione el usuario

        this.comboBoxOpciones.setItems(options);
        comboBoxOpciones.setOnAction(eh->{
            startComboBox();
        });

        this.btnGuardar.setOnAction(eh->{
            Image i=convertAnchorPaneToImage(emojiBlock);
           
            Emoji e=new Emoji(this.emojiEyes,this.emojiMouth, this.emojiBrows, this.emojiFace, i);
            Proyecto proyecto=new Proyecto(e);
            App.usuarioSeleccionado.getProyectos().addLast(proyecto);
            System.out.println("Emoji Agregado");
//             try {
//            FXMLLoader loader=new FXMLLoader(getClass().getResource("galleryWindow.fxml"));
//            Parent p= loader.load();
//            GalleryWindowController controller=loader.getController();
//            controller.llenarContenedor();
//        
//        } catch (Exception ex) {
//            ex.printStackTrace();
//        }
            
        });

        this.btnNext.setOnAction(eh->{
            runNext();
        });
        this.btnPrev.setOnAction(eh->{
            runPrev();
        });
        this.newFtButton.setOnAction(eh->{
           addFeature();
        });
        this.deleteFeatButton.setOnAction(eh->{
            deleteFeature();});
        
        //guardar proyecto
        this.btnGuardar.setOnAction(eh->{
            //Pide el nombre del proyecto
            //crea un diálogo de entrada de texto
            TextInputDialog dialog = new TextInputDialog();
            dialog.setTitle("Proyecto Nuevo");
            dialog.setHeaderText("Ingresa el nombre de tu proyecto");
            dialog.setContentText("Proyecto:");
            dialog.showAndWait().ifPresent(texto -> {
                //convierte la el emoji en imagen png y devuelve la imagen
                Image imagen = convertAnchorPaneToImage(emojiBlock);
                
                //Primero crear proyecto 
                Proyecto proy =crearProyecto(imagen);
                App.usuarioSeleccionado.getProyectos().addLast(proy);
                System.out.println(App.usuarioSeleccionado.getProyectos());
            });
        });
    }
    //meotod que carga panel y setean atributos iniciales 
    public void startComboBox(){
         this.contenedorScroll.getChildren().clear();
            comboMethod();
    }
    //metodo boton next
    public void runNext(){
        if (this.comboBoxOpciones.getValue()==null){
                Alert a=new Alert(Alert.AlertType.ERROR);
                a.setContentText("debe seleccionar un feature primero");
                a.showAndWait();
            }else{
                Image i=nodoF.getContent().getImage();
                
                this.ivSelected.setImage(i);
                nodoF=nodoF.getNext();
                
                
            }
    }
    //metodo boton previous
    public void runPrev(){
        if (this.comboBoxOpciones.getValue()==null){
                Alert a=new Alert(Alert.AlertType.ERROR);
                a.setContentText("debe seleccionar un feature primero");
                a.showAndWait();
            }else{
                nodoF=nodoF.getPrevious();
                Image i=nodoF.getContent().getImage();
                this.ivSelected.setImage(i);
                
            }
    }
    //metodo que añade nueva caracteristica 
    public void addFeature(){
         if (this.comboBoxOpciones.getValue()==null){
                Alert a=new Alert(Alert.AlertType.ERROR);
                a.setContentText("debe seleccionar un feature primero");
                a.showAndWait();
            }else{
               JFileChooser fileChooser = new JFileChooser();
                int result = fileChooser.showOpenDialog(null);
                FileNameExtensionFilter filter = new FileNameExtensionFilter("Archivos PNG", "png");
                fileChooser.setFileFilter(filter);
                if (result == JFileChooser.APPROVE_OPTION) {
                    File selectedFile = fileChooser.getSelectedFile();
                    
                    String filePath = "file:"+selectedFile.getAbsolutePath();
                    System.out.println(filePath);
                    Image i=new Image(filePath);
                    ImageView iv=new ImageView(i);
                    this.selectedList.addLast(iv);
                    this.contenedorScroll.getChildren().clear();
                    cargarFeatures(this.selectedList);
                }
                         
                
            }
    }
    //metodo que borra la caracteristica seleccionada
    public void deleteFeature(){
        if (this.comboBoxOpciones.getValue()==null){
                Alert a=new Alert(Alert.AlertType.ERROR);
                a.setContentText("debe seleccionar un feature primero");
                a.showAndWait();
            }else{
        this.ivSelected.setImage(null);
            NodeDCLL<ImageView> sub=nodoF.getNext();    
            this.selectedList.remove(this.nodoF.getContent());
            this.nodoF=sub;
            this.ivSelected.setImage(this.nodoF.getContent().getImage());
            this.contenedorScroll.getChildren().clear();
            cargarFeatures(this.selectedList);
    }
        }
            
    //metodo del comboBox
    public void comboMethod(){
        String seleccionado=comboBoxOpciones.getValue().toString();
            if (seleccionado.equals("ojos")){
                cargarFeatures(ojos);
                this.nodoF=this.ojos.getNode();
  
                this.ivSelected=this.emojiEyes;

                this.ivSelected=this.emojiEyes; //posiciona el imageview de la opcion escogida

                this.selectedList=this.ojos;
            }
             if (seleccionado.equals("boca")){
                cargarFeatures(bocas);
                this.nodoF=this.bocas.getNode();
                this.ivSelected=this.emojiMouth;
                this.selectedList=this.bocas;
                
            }
              if (seleccionado.equals("cara")){
                cargarFeatures(caras);
                this.nodoF=this.caras.getNode();
                this.ivSelected=this.emojiFace;
                this.selectedList=this.caras;
                
            }
               if (seleccionado.equals("ceja")){
                cargarFeatures(cejas);
                this.nodoF=this.cejas.getNode();
                this.ivSelected=this.emojiBrows;
                this.selectedList=this.cejas;
                
            }
                if (seleccionado.equals("accesorios")){
                cargarFeatures(extras);
                
                
            }
            
    }
    
    //metodo que carga la lista doble enlazada en el panel 
    public void cargarFeatures(DCLList<ImageView> lista){
        for (int i=0;i<lista.size();i++){
              
            ImageView iv=lista.get(i);
            iv.setFitHeight(60);
            iv.setFitWidth(60);
            
            iv.setOnMouseClicked(eh->{
               
                this.ivSelected.setImage(iv.getImage());
                nodoF=lista.getNodeByContent(iv);
                
            });
            this.contenedorScroll.getChildren().add(iv);
        }
    }
    //metodo que llena los double linkedlist
    //Se cambio el arraylist al nuestro
    public void llenarLists(){
        File directorio = new File("src\\main\\java\\imagenes\\eyes");
        ArrayList<File> imagenes = cargar(directorio);
        for(File file: imagenes){
            
            try {
                Image i=Main.crearImagen(file);
                ImageView iv=new ImageView(i);
                this.ojos.addLast(iv);
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            }
        }
        File directorioex = new File("src\\main\\java\\imagenes\\accessories");
        ArrayList<File> imagenesex = cargar(directorioex);
        for(File file: imagenesex){
            
            try {
                Image i=Main.crearImagen(file);
                ImageView iv=new ImageView(i);
                this.extras.addLast(iv);
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            }
        }
        File directoriom = new File("src\\main\\java\\imagenes\\mouth");
        ArrayList<File> imagenesm = cargar(directoriom);
        for(File file: imagenesm){
            
            try {
                Image i=Main.crearImagen(file);
                ImageView iv=new ImageView(i);
                this.bocas.addLast(iv);
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            }
        }
        File directoriobrows = new File("src\\main\\java\\imagenes\\eyebrows");
        ArrayList<File> imagenesbrows = cargar(directoriobrows);
        for(File file: imagenesbrows){
            
            try {
                Image i=Main.crearImagen(file);
                ImageView iv=new ImageView(i);
                this.cejas.addLast(iv);
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            }
        }
        File directoriof = new File("src\\main\\java\\imagenes\\faces");
        ArrayList<File> imagenesf = cargar(directoriof);
        for(File file: imagenesf){
            
            try {
                Image i=Main.crearImagen(file);
                ImageView iv=new ImageView(i);
                this.caras.addLast(iv);
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            }
        }

       
    
      } 
   //metodo que convierte el contenedor del emoji en imagen
   public static Image convertAnchorPaneToImage(AnchorPane anchorPane) {
    WritableImage snapshot = anchorPane.snapshot(new SnapshotParameters(), null);
    BufferedImage bufferedImage = SwingFXUtils.fromFXImage(snapshot, null);
    ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
    try {
        ImageIO.write(bufferedImage, "png", outputStream);
        byte[] imageData = outputStream.toByteArray();
        ByteArrayInputStream inputStream = new ByteArrayInputStream(imageData);
        
        //Directorio donde se guardan las imagenes
        String resourcePath = "src\\main\\resources\\ImagenesProyectos\\";
        
        //Aqui habria que siempre ponerle un nombre unico a cada imagen
        String imagePath = resourcePath + ".png";
        
        //lo guardamos en un archivo que pueda ser enviado cuando se escriba la imagen
        File outputFile = new File(imagePath);
        //se escribe la imagen
        ImageIO.write(bufferedImage, "png", outputFile);
        return new Image(inputStream);
    } catch (IOException e) {
        e.printStackTrace();
    }
    return null;
}
   public Proyecto crearProyecto( Image portada){
       //tengo que poner cada uno de los url
       
      
       Emoji emoji = new Emoji(emojiEyes,emojiMouth,emojiBrows,emojiFace,portada);
       //Creo el proyecto
       Proyecto proy = new Proyecto(emoji);
       return proy;
   }
   //metodo que se acciona cuando un proyecto es abierto
   public void cargarProyecto(Proyecto proy){
        try {
            //aqui me pasa el proyecto debo colocar los image view como los tiene
            //cargamos las caracteristicas del emoji/ o podemos ir viendo en la lista
            Image ojosIm = Main.crearImagen(new File(proy.getContent().getEyes_url().getImage().getUrl()));
            Image bocaIm = Main.crearImagen(new File(proy.getContent().getMouth_url().getImage().getUrl()));
            Image cejasIm = Main.crearImagen(new File(proy.getContent().getBrows_url().getImage().getUrl()));
            Image caraIm = Main.crearImagen(new File(proy.getContent().getFace_url().getImage().getUrl()));
            Image accIm = Main.crearImagen(new File(proy.getContent().getAccesory().getImage().getUrl()));
            
            //se la agrega al atributo correspondiente de nuestro lienzo
            emojiEyes.setImage(ojosIm);
            emojiMouth.setImage(bocaIm);
            emojiBrows.setImage(cejasIm);
            emojiFace.setImage(caraIm);
            //faltaria el accesorio++++=+++++++++
            
        } catch (FileNotFoundException ex) {
            ex.printStackTrace();
        }
   }

    public ImageView getEmojiFace() {
        return emojiFace;
    }

    public void setEmojiFace(ImageView emojiFace) {
        this.emojiFace = emojiFace;
    }

    public ImageView getEmojiBrows() {
        return emojiBrows;
    }

    public void setEmojiBrows(ImageView emojiBrows) {
        this.emojiBrows = emojiBrows;
    }

    public ImageView getEmojiEyes() {
        return emojiEyes;
    }

    public void setEmojiEyes(ImageView emojiEyes) {
        this.emojiEyes = emojiEyes;
    }

    public ImageView getEmojiMouth() {
        return emojiMouth;
    }

    public void setEmojiMouth(ImageView emojiMouth) {
        this.emojiMouth = emojiMouth;
    }

   
   
}
