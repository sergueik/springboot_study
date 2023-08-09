package com.pocisoft.eventsapi.data.viewmodel;

import java.util.Date;

import com.pocisoft.eventsapi.data.EventSubscription;

public class EventSubscriptionViewModel {
    private int id;
    private int userId;
    private int eventId;
    private Date createdAt;
    private UserAccountViewModel userAccount;
    private EventViewModel event;

    public EventSubscriptionViewModel() {
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public int getUserId() {
        return userId;
    }

    public void setUserId(int userId) {
        this.userId = userId;
    }

    public int getEventId() {
        return eventId;
    }

    public void setEventId(int eventId) {
        this.eventId = eventId;
    }

    public Date getCreatedAt() {
        return createdAt;
    }

    public void setCreatedAt(Date createdAt) {
        this.createdAt = createdAt;
    }

    public UserAccountViewModel getUserAccount() {
        return userAccount;
    }

    public void setUserAccount(UserAccountViewModel userAccount) {
        this.userAccount = userAccount;
    }

    public EventViewModel getEvent() {
        return event;
    }

    public void setEvent(EventViewModel event) {
        this.event = event;
    }
    public static EventSubscriptionViewModel fromEventSubscription(EventSubscription eventSubscription) {
        EventSubscriptionViewModel viewModel = new EventSubscriptionViewModel();
        viewModel.setId(eventSubscription.getId());
        viewModel.setUserId(eventSubscription.getUserId());
        viewModel.setEventId(eventSubscription.getEventId());
        viewModel.setCreatedAt(eventSubscription.getCreatedAt());

        // Optionally map the UserAccount and Event properties
        viewModel.setUserAccount(UserAccountViewModel.fromUserAccount(eventSubscription.getUserAccount()));
        viewModel.setEvent(EventViewModel.fromEvent(eventSubscription.getEvent()));

        return viewModel;
    }
}
