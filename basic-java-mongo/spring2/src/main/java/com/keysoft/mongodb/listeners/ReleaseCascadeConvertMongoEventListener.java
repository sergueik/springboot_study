package com.keysoft.mongodb.listeners;

import com.keysoft.mongodb.model.Release;
import org.bson.types.ObjectId;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.data.mongodb.core.MongoTemplate;
import org.springframework.data.mongodb.core.mapping.event.AbstractMongoEventListener;
import org.springframework.data.mongodb.core.mapping.event.BeforeConvertEvent;

public class ReleaseCascadeConvertMongoEventListener extends AbstractMongoEventListener<Object> {
    @Override
    public void onBeforeConvert(BeforeConvertEvent<Object> event) {
        //The _id field is a required field of the parent document, and is typically not necessary or present in embedded documents
        //However, I want the embedded documents to have an id, so I'm catching the event and adding ids before the Tickets are saved with the Release
        Object source = event.getSource();
        if ((source instanceof Release) ) {
            ((Release) source).getTickets().forEach(ticket -> {
               ticket.setId(new ObjectId().toString());
            });
        }
    }
}
