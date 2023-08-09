package com.pocisoft.eventsapi.data.viewmodel;

import java.util.Date;

import com.pocisoft.eventsapi.data.Event;

public class EventViewModel {
    private int id;
    private String name;
    private String description;
    private int maxCapacity;
    private Date date;
    private String organizer;
    private String location;
    private String startTime;
    private int numberOfHours;
    private int currentCapacity;
    private int userId;

    public EventViewModel() {
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

    public int getCurrentCapacity() {
        return currentCapacity;
    }

    public void setCurrentCapacity(int currentCapacity) {
        this.currentCapacity = currentCapacity;
    }
    public int getUserId() {
        return userId;
    }

    public void setUserId(int userId) {
        this.userId = userId;
    }
    public static EventViewModel fromEvent(Event event) {
        EventViewModel viewModel = new EventViewModel();
        viewModel.setId(event.getId());
        viewModel.setName(event.getName());
        viewModel.setDescription(event.getDescription());
        viewModel.setMaxCapacity(event.getMaxCapacity());
        viewModel.setDate(event.getDate());
        viewModel.setOrganizer(event.getOrganizer());
        viewModel.setLocation(event.getLocation());
        viewModel.setStartTime(event.getStartTime());
        viewModel.setNumberOfHours(event.getNumberOfHours());
        viewModel.setCurrentCapacity(event.getCurrentCapacity());
        viewModel.setUserId(event.getUserId());

        return viewModel;
    }
}
