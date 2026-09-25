/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package Classes;

import TDASimplement.ArrayList;

/**
 *
 * @author USUARIO
 */
public class Usuario {
    private String username;
    private String password;
    private ArrayList<Proyecto> proyectos;

    public Usuario(String username, String password) {
        this.username = username;
        this.password = password;
        proyectos=new ArrayList<>();
    }

    public ArrayList<Proyecto> getProyectos() {
        return proyectos;
    }

    public void setProyectos(ArrayList<Proyecto> proyectos) {
        this.proyectos = proyectos;
    }
    

    public String getUsername() {
        return username;
    }

    public void setUsername(String username) {
        this.username = username;
    }

    public String getPassword() {
        return password;
    }

    public void setPassword(String password) {
        this.password = password;
    }
    
  @Override 
    public boolean equals(Object o){
    if (this==o){
    return true;
    }
  if (o!=null && o instanceof Usuario){
    Usuario p=(Usuario) o;
    return this.username.equals(p.username) && this.password.equals(p.password);
  }
      return false;
    }
}   

