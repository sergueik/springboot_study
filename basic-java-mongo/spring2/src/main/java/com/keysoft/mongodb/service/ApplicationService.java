package com.keysoft.mongodb.service;

import com.keysoft.mongodb.model.Application;
import org.springframework.data.mongodb.core.query.Query;
import org.springframework.data.mongodb.core.query.Update;

public interface ApplicationService {
    void addNewApplicationWTemplate(Application application);
    Application findByIdTemplate(String id);
    void deleteWTemplate(Application application);
    void updateApplicationWTemplate(Application application);
}


