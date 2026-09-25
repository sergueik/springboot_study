/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pruebas;

import TDASimplement.ArrayList;
import TDASimplement.DCLList;

import com.mycompany.emojimaker.EmojiLienzoController;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.net.MalformedURLException;

import java.util.Iterator;
import javafx.scene.image.Image;

/**
 *
 * @author noeli
 */
public class Main {
    
    
    public static void main(String[] args) throws MalformedURLException, FileNotFoundException {
        DCLList<String> lista = new DCLList();
        
        
        lista.addLast("1");
        lista.addLast("2");
        lista.addLast("3");
        lista.addLast("4");
        lista.addLast("5");

        //Prueba remove index 
        lista.remove("2");

        Iterator<String> it = lista.iterator();
        
        while (it.hasNext()) {
            System.out.println(it.next());
        }
    
        /*
        PRUEBA DE CARGAR IMAGENES DEL DIRECTORIO 
        */
        
        File directorio = new File("src\\main\\java\\imagenes\\eyes");
        ArrayList<File> imagenes = cargar(directorio);
        System.out.println("hol");
        //esas files pasarla a image
        
        for(File file: imagenes){
            
            System.out.println("Hola");
            //pasar a imagen
        }

    
    }
    //Se ha cambiado el arrayList a el nuestro en el import 
    public static ArrayList<File> cargar(File directorio){
        
        File[] arregloImagenes = directorio.listFiles();
        
        ArrayList<File> imagenes = new ArrayList<>();
        
        for(File foto: arregloImagenes){
            imagenes.addLast(foto);
        }
        
        return imagenes;
     }
    
    public static Image crearImagen(File img) throws FileNotFoundException{

        Image image = new Image(new FileInputStream(img));  
        return image;
    }
        
    

}
