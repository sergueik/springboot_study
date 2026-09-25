/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Interface.java to edit this template
 */
package Classes;

import TDASimplement.DCLList;
import java.io.File;

/**
 *
 * @author melis
 * @param <E> al cargar una coleccion se debe traer a una doubly linked list con objeto image view
 * y se debe pasar como un formato de donde se encuentran las imagenes
 */
public interface CargarColeccion<E> {
    
    //esta cargaria solo 1 imagen, como hago varias???
    public DCLList<E> cargarImagenes(File directorio);
}
