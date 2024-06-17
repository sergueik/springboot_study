package com.keysoft.mongodb.model;

import java.time.LocalDate;
import java.util.List;

import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.Transient;
import org.springframework.data.mongodb.core.mapping.DBRef;
import org.springframework.data.mongodb.core.mapping.Document;
import org.springframework.data.mongodb.core.mapping.Field;

@Document
public class Release {

    @Id
    private String id;
    private String name;
    private String description;
    private List<Ticket> tickets;

    @Transient
    private Double estimatedCosts;

    public Release() { }

    public Release(String name, String description, List<Ticket> tickets) {
        this.name = name;
        this.description = description;
        this.tickets = tickets;
        this.estimatedCosts = 0.00;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public List<Ticket> getTickets() {
        return tickets;
    }

    public void setTickets(List<Ticket> tickets) {
        this.tickets = tickets;
    }

    public Double getEstimatedCosts() {
        return tickets.size() * 15.50;
    }

    public void setEstimatedCosts(Double estimatedCosts) {
        this.estimatedCosts = estimatedCosts;
    }
}
