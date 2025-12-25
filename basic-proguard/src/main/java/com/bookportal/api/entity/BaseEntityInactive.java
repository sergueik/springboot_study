package com.bookportal.api.entity;

import com.fasterxml.jackson.annotation.JsonIgnore;
import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;
import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.mongodb.core.mapping.Document;

import java.io.Serializable;
import java.util.Date;

@Getter
@Setter
@AllArgsConstructor
@NoArgsConstructor
@Document
public class BaseEntityInactive implements Serializable {
    @Id
    private String id;

    @JsonIgnore
    @CreatedDate
    private Date createDate;

    @JsonIgnore
    @LastModifiedDate
    private Date updateDate;

    private boolean active;

    @JsonIgnore
    private String operationType;

    @Override
    public String toString() {
        return "BaseEntity{" +
                "id=" + id +
                ", createDate=" + createDate +
                ", updateDate=" + updateDate +
                ", active=" + active +
                ", operationType='" + operationType + '\'' +
                '}';
    }
}
