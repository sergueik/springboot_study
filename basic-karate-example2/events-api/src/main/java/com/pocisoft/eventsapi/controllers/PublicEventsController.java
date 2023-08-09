package com.pocisoft.eventsapi.controllers;

import com.pocisoft.eventsapi.data.APIResponse;
import com.pocisoft.eventsapi.data.Event;
import com.pocisoft.eventsapi.data.EventDAO;
import com.pocisoft.eventsapi.data.viewmodel.EventViewModel;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/publicevents")
public class PublicEventsController {

    @Autowired
    private EventDAO eventDAO;
      
  
    @GetMapping()
    public ResponseEntity<APIResponse<List<EventViewModel>>> getEvents() {
        try {
        	
            List<EventViewModel> events = eventDAO.getAllEvents().stream()
                    .map(EventViewModel::fromEvent)
                    .collect(Collectors.toList());
            return ResponseEntity.ok(APIResponse.success(events));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<List<EventViewModel>>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }
    @GetMapping("/{id}")
    public ResponseEntity<APIResponse<EventViewModel>> getEvent(@PathVariable int id) {
        try {
        	
            Event event = eventDAO.getEvent(id);
            if (event != null) {
                return ResponseEntity.ok(APIResponse.success(EventViewModel.fromEvent(event)));
            } else {
                return ResponseEntity.status(HttpStatus.NOT_FOUND)
                        .body(APIResponse.<EventViewModel>failure(HttpStatus.NOT_FOUND.value(), "Event not found"));
            }
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<EventViewModel>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }
   
}


