package org.example.ua.entity;

import lombok.Data;

import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;
import javax.persistence.Transient;
import java.io.Serializable;
import java.util.List;
import java.util.stream.Collectors;
import java.util.stream.Stream;

@Entity
@Table(name = "user_table")
@Data
public class User implements Serializable {

    private static final String ROLES_DELIMITER = ",";

    public static final Role DEFAULT_ROLE = Role.ROLE_USER;

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String name;

    private String roles;

    private String email;

    private String password;

    @Transient
    public List<Role> getRolesList() {
        return Stream.of(roles.split(ROLES_DELIMITER))
                .map(Role::valueOf)
                .collect(Collectors.toList());
    }

    @Transient
    public void setRolesList(Role... roles) {
        this.roles = Stream.of(roles)
                .map(Role::toString)
                .collect(Collectors.joining(ROLES_DELIMITER));
    }

    public enum Role {
        ROLE_ADMIN, ROLE_USER
    }
}
