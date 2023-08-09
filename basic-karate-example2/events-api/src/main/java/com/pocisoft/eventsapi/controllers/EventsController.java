package com.pocisoft.eventsapi.controllers;

import com.pocisoft.eventsapi.data.APIException;
import com.pocisoft.eventsapi.data.APIResponse;
import com.pocisoft.eventsapi.data.Event;
import com.pocisoft.eventsapi.data.EventDAO;
import com.pocisoft.eventsapi.data.models.EventDTO;
import com.pocisoft.eventsapi.data.viewmodel.EventSubscriptionViewModel;
import com.pocisoft.eventsapi.data.viewmodel.EventViewModel;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.security.access.prepost.PreAuthorize;
import org.springframework.web.bind.annotation.*;

import java.util.List;
import java.util.stream.Collectors;

@RestController
@RequestMapping("/api/events")
@PreAuthorize("isAuthenticated()")
public class EventsController extends ControllerBase {

    @Autowired
    private EventDAO eventDAO;
      
  
    @GetMapping()
    public ResponseEntity<APIResponse<List<EventViewModel>>> getEvents(
    		@RequestParam(name = "eventName", required = false) String eventName) {
        try {
        	
            List<EventViewModel> events = eventDAO.getEvents(getCurrentUserId(),eventName).stream()
                    .map(EventViewModel::fromEvent)
                    .collect(Collectors.toList());;
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

    @PostMapping("")
    public ResponseEntity<APIResponse<EventViewModel>> addEvent(@RequestBody EventDTO eventDTO) {
        try {
           Event newEvent= eventDAO.addEvent(getCurrentUserId(),eventDTO);
            return ResponseEntity.status(HttpStatus.CREATED).body(APIResponse.success(EventViewModel.fromEvent(newEvent)));
        } catch (APIException e) {
            e.printStackTrace();
            return ResponseEntity.status(e.getStatusCode())
                    .body(APIResponse.<EventViewModel>failure(e.getStatusCode(), e.getMessage()));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<EventViewModel>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }
    
    @PutMapping("/{id}")
    public ResponseEntity<APIResponse<EventViewModel>> updateEvent(@PathVariable int id, @RequestBody EventDTO eventDTO) {
        try {
        	Event event = eventDAO.updateEvent(getCurrentUserId(),id, eventDTO);
            return ResponseEntity.ok(APIResponse.success(EventViewModel.fromEvent(event)));
        } catch (APIException e) {
            e.printStackTrace();
            return ResponseEntity.status(e.getStatusCode())
                    .body(APIResponse.<EventViewModel>failure(e.getStatusCode(), e.getMessage()));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<EventViewModel>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<APIResponse<EventViewModel>> deleteEvent(@PathVariable int id) {
        try {
        	 eventDAO.deleteEvent(getCurrentUserId(),id);
            return ResponseEntity.ok(APIResponse.success(null));
        } catch (APIException e) {
            e.printStackTrace();
            return ResponseEntity.status(e.getStatusCode())
                    .body(APIResponse.<EventViewModel>failure(e.getStatusCode(), e.getMessage()));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<EventViewModel>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }
    @GetMapping("/{id}/subscriptions")
    public ResponseEntity<APIResponse<List<EventSubscriptionViewModel>>> getSubscriptionsByEvent(@PathVariable int id) {
        try {
        	
            List<EventSubscriptionViewModel> events = eventDAO.getSubscriptionsByEvent(getCurrentUserId()).stream()
                    .map(EventSubscriptionViewModel::fromEventSubscription)
                    .collect(Collectors.toList());;;
            return ResponseEntity.ok(APIResponse.success(events));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<List<EventSubscriptionViewModel>>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }
}


