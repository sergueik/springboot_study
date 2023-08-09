package com.pocisoft.eventsapi.data.viewmodel;

import com.pocisoft.eventsapi.data.UserAccount;

public class UserAccountViewModel {
    private int id;
    private String firstName;
    private String lastName;
    private String email;

    public UserAccountViewModel() {
    }

    public int getId() {
        return id;
    }

    public void setId(int id) {
        this.id = id;
    }

    public String getFirstName() {
        return firstName;
    }

    public void setFirstName(String firstName) {
        this.firstName = firstName;
    }

    public String getLastName() {
        return lastName;
    }

    public void setLastName(String lastName) {
        this.lastName = lastName;
    }

    public String getEmail() {
        return email;
    }

    public void setEmail(String email) {
        this.email = email;
    }
    public static UserAccountViewModel fromUserAccount(UserAccount userAccount) {
        UserAccountViewModel viewModel = new UserAccountViewModel();
        viewModel.setId(userAccount.getId());
        viewModel.setFirstName(userAccount.getFirstName());
        viewModel.setLastName(userAccount.getLastName());
        viewModel.setEmail(userAccount.getEmail());
        return viewModel;
    }
}
