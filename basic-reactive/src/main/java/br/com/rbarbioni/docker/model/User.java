package br.com.rbarbioni.docker.model;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.TypeAlias;
import org.springframework.data.mongodb.core.mapping.Document;

import javax.validation.constraints.*;
import java.io.Serializable;

/**
 * Created by renan on 23/05/17.
 */

@Document(collection = "user")
@TypeAlias("user")
public class User implements Serializable{

    private static final long serialVersionUID = 8860980640517255440L;

    @Id
    private String id;

    @NotNull
    @NotEmpty
    private String name;

    @NotEmpty
    @Email
    private String email;

    protected User (){}

    public User(String name, String email) {
        this();
        this.name = name;
        this.email = email;
    }

    public String getId() {
        return id;
    }

    public String getName() {
        return name;
    }

    public String getEmail() {
        return email;
    }
}
