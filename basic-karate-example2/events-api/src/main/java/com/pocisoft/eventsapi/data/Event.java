package com.pocisoft.eventsapi.data;

import javax.persistence.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

@Entity
@Table(name = "events")
public class Event {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private int id;

    @Column(name = "name", nullable = false)
    private String name;

    @Column(name = "description")
    private String description;

    @Column(name = "maxCapacity", nullable = false)
    private int maxCapacity;

    @Column(name = "date", nullable = false)
    private Date date;

    @Column(name = "organizer", nullable = false)
    private String organizer;

    @Column(name = "location", nullable = false)
    private String location;

    @Column(name = "startTime", nullable = false)
    private String startTime;

    @Column(name = "numberOfHours", nullable = false)
    private int numberOfHours;

    @Column(name = "userId", nullable = false)
    private int userId;
    
    @OneToMany(mappedBy = "event", cascade = CascadeType.ALL, orphanRemoval = false)
    private List<EventSubscription> eventSubscriptions = new ArrayList<>();

  

    @Transient
    private int currentCapacity;

    public List<EventSubscription> getEventSubscriptions() {
        return eventSubscriptions;
    }

    public void setEventSubscriptions(List<EventSubscription> eventSubscriptions) {
        this.eventSubscriptions = eventSubscriptions;
    }

    public int getCurrentCapacity() {
        return currentCapacity;
    }

    public void setCurrentCapacity(int currentCapacity) {
        this.currentCapacity = currentCapacity;
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
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

    public int getMaxCapacity() {
        return maxCapacity;
    }

    public void setMaxCapacity(int maxCapacity) {
        this.maxCapacity = maxCapacity;
    }

    public Date getDate() {
        return date;
    }

    public void setDate(Date date) {
        this.date = date;
    }

    public String getOrganizer() {
        return organizer;
    }

    public void setOrganizer(String organizer) {
        this.organizer = organizer;
    }

    public String getLocation() {
        return location;
    }

    public void setLocation(String location) {
        this.location = location;
    }

    public String getStartTime() {
        return startTime;
    }

    public void setStartTime(String startTime) {
        this.startTime = startTime;
    }

    public int getNumberOfHours() {
        return numberOfHours;
    }

    public void setNumberOfHours(int numberOfHours) {
        this.numberOfHours = numberOfHours;
    }

    public int getUserId() {
        return userId;
    }

    public void setUserId(int userId) {
        this.userId = userId;
    }
}
