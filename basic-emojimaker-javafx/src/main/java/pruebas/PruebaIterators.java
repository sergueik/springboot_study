/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package pruebas;

import TDASimplement.ArrayList;

/**
 *
 * @author melis
 */
public class PruebaIterators {
    public static void main(String[] args) {
        ArrayList<Integer> enteros = new ArrayList<>();
        enteros.addLast(1);
        enteros.addLast(2);
        

        for(Integer entero: enteros){
            System.out.println(entero);
        }
    }
    
}
