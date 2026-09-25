/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package TDASimplement;

import java.util.Iterator;

/**
 *
 * @author USUARIO
 * @param <E>
 */
public class DCLList<E> implements List<E>, Iterable<E>{

    private NodeDCLL<E> last;
    
    public DCLList(){
        last = null;
    }
    
    

    @Override
    public boolean addLast(E element) {
        NodeDCLL<E> nodo = new NodeDCLL(element);
        
        if ( this.size() == 0){
            nodo.setNext(nodo);
            nodo.setPrevious(nodo);
            this.last = nodo;
            
            return true;
        }     
        
        nodo.setNext(this.last.getNext());
        this.last.getNext().setPrevious(nodo);
        this.last.setNext(nodo);
        nodo.setPrevious(this.last);
        this.last=nodo;   
        return true;
    }
    

    @Override
    public int size() {
        //contador
        int contador = 0;
        
        //si esta vacio
        if(last == null){
            return contador;
        }
        
        //caso general
        NodeDCLL<E> nodoViajero=this.last;
        contador++;
        
        //Cannot invoke "TDASimplement.NodeDCLL.equals(TDASimplement.NodeDCLL)" because "nodoViajero" is null
        
        
        //aqui va un equals en la condicion o puede ser nodoViajero != last ???
        for(nodoViajero =nodoViajero.getNext(); !(nodoViajero.equals(last)); nodoViajero = nodoViajero.getNext()){
            contador ++;
        }
        
        return contador;
    }

    @Override
    public E get(int i) {
        E elemento=null;
        int cont=0;
        if (i>=0 && i<this.size()){
            if (i==0){
                elemento=this.last.getNext().getContent();
            }
            else if (i==this.size()-1){
                elemento=this.last.getContent();
            }
            else {
                NodeDCLL<E> nodoViajero=this.last;
                
                for(nodoViajero =nodoViajero.getNext(); !(nodoViajero.equals(last)); nodoViajero = nodoViajero.getNext()){
                    if (cont==i){
                        elemento=nodoViajero.getContent();
                        
                    }
                    cont++;
                    
                }
            }
        }
        else {
            throw new IndexOutOfBoundsException("indice fuera de rango");
        }
        return elemento;
    }
    
    public NodeDCLL getNode(){
        return this.last.getNext();
    }
    public NodeDCLL getNodeByContent(E content){
        NodeDCLL nodo=null;
        NodeDCLL<E> nodoViajero=this.last;
        if (this.last.getContent().equals(content)){
            nodo=this.last;
        }
        else{
        for(nodoViajero =nodoViajero.getNext(); !(nodoViajero.equals(last)); nodoViajero = nodoViajero.getNext()){
                    if (nodoViajero.getContent().equals(content)){
                       nodo=nodoViajero;
                        
                    }
                    }         
        }            
        return nodo;
    }
    @Override

    public boolean isEmpty() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public void clear() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public boolean addFirst(E element) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

   
    @Override
    public E removeFirst() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public E removeLast() {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public boolean add(int index, E element) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    @Override
    public E remove(int index) {
      E contenido=null;
      
      return contenido;
    }

   
    @Override
    public E set(int index, E element) {
        throw new UnsupportedOperationException("Not supported yet."); // Generated from nbfs://nbhost/SystemFileSystem/Templates/Classes/Code/GeneratedMethodBody
    }

    public boolean remove(E element) {
        
        //tendriamos que ir iterando uno a uno e ir comparando
        NodeDCLL<E> nodo = new NodeDCLL<>(element);
        
        NodeDCLL<E> nodoViajero = last;
        
        //ver el size e ir iterando
        for(int i = 0;i < size() ;i++){
            
            //va pasando cada elemento a ver si es igual y se remueve
            if(nodoViajero.equals(nodo)){
                //setea el siguiente del anterior
                nodoViajero.getPrevious().setNext(nodoViajero.getNext());
                //setea el anterior del siguiente
                nodoViajero.getNext().setPrevious(nodoViajero.getPrevious());
                
                //en caso de que sea el ultimo
                if(nodoViajero.equals(last)){
                    last = nodoViajero.getPrevious();
                }
               
                return true;
            }
            
            nodoViajero = nodoViajero.getNext();
        }
        return false;
    }

    @Override
    public String toString() {
        NodeDCLL<E> nodoViajero = last;
        String cadena = "";
        
        E contenido = nodoViajero.getContent();

        for(nodoViajero = nodoViajero.getNext(); !nodoViajero.equals(last); nodoViajero = nodoViajero.getNext()){
            cadena += nodoViajero.getContent() + ", ";
        }
        
        cadena += contenido;
        return cadena;
    }

    @Override
    public Iterator<E> iterator() {
        Iterator<E> it = new Iterator<E>() {
              
            NodeDCLL<E> cursor = last.getNext();
            
            
            @Override
            public boolean hasNext() {
                return cursor != null;
            }

            @Override
            public E next() {
                E e = cursor.getContent();
                if(cursor.getNext() == last.getNext()){
                    cursor = null;
                }else{
                    cursor = cursor.getNext();
                }
                return e;
            }
        };
        
        return it;
        
    }
    
    
    
}
