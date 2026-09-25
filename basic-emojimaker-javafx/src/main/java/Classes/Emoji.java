/*
 * Click nbfs://nbhost/SystemFileSystem/Templates/Licenses/license-default.txt to change this license
 * Click nbfs://nbhost/SystemFileSystem/Templates/Classes/Class.java to edit this template
 */
package Classes;

import javafx.scene.image.Image;
import javafx.scene.image.ImageView;

/**
 *
 * @author USUARIO
 */
public class Emoji {
    private ImageView eyes_url;
    private ImageView   mouth_url;
    private ImageView  brows_url;
    private ImageView   face_url;
    private ImageView   accesory; 
    private Image portada=null;
    public Emoji(ImageView  eyes_url, ImageView  mouth_url, ImageView  brows_url,ImageView face_url, Image portada) {
        this.eyes_url = eyes_url;
        this.mouth_url = mouth_url;
        this.brows_url = brows_url;
        this.face_url = face_url;
        this.portada=portada;
        
    }

    public Image getPortada() {
        return portada;
    }

    public void setPortada(Image portada) {
        this.portada = portada;
    }

    public Emoji(ImageView eyes_url, ImageView  mouth_url, ImageView  brows_url, ImageView  face_url,ImageView  accesory, Image portada) {
        this(eyes_url, mouth_url, brows_url, face_url,portada);
        this.accesory = accesory;
    }

    public ImageView getEyes_url() {
        return eyes_url;
    }

    public void setEyes_url(ImageView eyes_url) {
        this.eyes_url = eyes_url;
    }

    public ImageView getMouth_url() {
        return mouth_url;
    }

    public void setMouth_url(ImageView mouth_url) {
        this.mouth_url = mouth_url;
    }

    public ImageView getBrows_url() {
        return brows_url;
    }

    public void setBrows_url(ImageView brows_url) {
        this.brows_url = brows_url;
    }

    public ImageView getFace_url() {
        return face_url;
    }

    public void setFace_url(ImageView face_url) {
        this.face_url = face_url;
    }

    public ImageView getAccesory() {
        return accesory;
    }

    public void setAccesory(ImageView accesory) {
        this.accesory = accesory;
    }

   
    
    
}
