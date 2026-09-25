/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package Classes;

import TDASimplement.ArrayList;
import TDASimplement.DCLList;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import javafx.scene.image.Image;

/**
 *
 * @author melis
 */
public class ColeccionImagenes implements CargarColeccion{

    //File directorio = "path donde esta ubicado"
    @Override
    public DCLList cargarImagenes(File directorio) {
        File[] arregloImagenes = directorio.listFiles();
        
        ArrayList<File> imagenes = new ArrayList<>();
        
        for(File foto: arregloImagenes){
            imagenes.addLast(foto);
        }
        
        DCLList<Image> coleccion = new DCLList<>();
        
        //hay que hacer iterable de arrayList 
        for(int i=0;i<imagenes.size();i++){
            
            try {
                //se guarda en la DLlist
                coleccion.addLast(crearImagen(imagenes.get(i)));
                //pasar a imagen
            } catch (FileNotFoundException ex) {
                ex.printStackTrace();
            }
        }
        
        return coleccion;
        
    }
    
    private Image crearImagen(File img) throws FileNotFoundException{
        Image image = new Image(new FileInputStream(img));  
        return image;
    }
    
    
    
    
}
