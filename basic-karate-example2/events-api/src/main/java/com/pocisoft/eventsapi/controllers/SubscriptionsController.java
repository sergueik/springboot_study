package com.pocisoft.eventsapi.controllers;

import com.pocisoft.eventsapi.data.APIException;
import com.pocisoft.eventsapi.data.APIResponse;
import com.pocisoft.eventsapi.data.Event;
import com.pocisoft.eventsapi.data.EventDAO;
import com.pocisoft.eventsapi.data.EventSubscription;
import com.pocisoft.eventsapi.data.SubscriptionsDAO;
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
@RequestMapping("/api/subscriptions")
@PreAuthorize("isAuthenticated()")
public class SubscriptionsController extends ControllerBase {

    @Autowired
    private SubscriptionsDAO eventDAO;
      
  
    @GetMapping()
    public ResponseEntity<APIResponse<List<EventSubscriptionViewModel>>> getSubscriptions() {
        try {
        	
            List<EventSubscriptionViewModel> events = eventDAO.getSubscriptionsByUser(getCurrentUserId()).stream()
                    .map(EventSubscriptionViewModel::fromEventSubscription)
                    .collect(Collectors.toList());;;
            return ResponseEntity.ok(APIResponse.success(events));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<List<EventSubscriptionViewModel>>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }

   
    
    @PostMapping("/{id}")
    public ResponseEntity<APIResponse<EventSubscriptionViewModel>> addSubscription(@PathVariable int id) {
        try {
        	EventSubscription event = eventDAO.addSubscription(getCurrentUserId(),id);
            return ResponseEntity.ok(APIResponse.success(EventSubscriptionViewModel.fromEventSubscription(event)));
        } catch (APIException e) {
            e.printStackTrace();
            return ResponseEntity.status(e.getStatusCode())
                    .body(APIResponse.<EventSubscriptionViewModel>failure(e.getStatusCode(), e.getMessage()));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<EventSubscriptionViewModel>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }

    @DeleteMapping("/{id}")
    public ResponseEntity<APIResponse<EventSubscriptionViewModel>> deleteSubscription(@PathVariable int id) {
        try {
        	 eventDAO.deleteSubscription(getCurrentUserId(),id);
            return ResponseEntity.ok(APIResponse.success(EventSubscriptionViewModel.fromEventSubscription(null)));
        } catch (APIException e) {
            e.printStackTrace();
            return ResponseEntity.status(e.getStatusCode())
                    .body(APIResponse.<EventSubscriptionViewModel>failure(e.getStatusCode(), e.getMessage()));
        } catch (Exception e) {
            e.printStackTrace();
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body(APIResponse.<EventSubscriptionViewModel>failure(HttpStatus.INTERNAL_SERVER_ERROR.value(), e.getMessage()));
        }
    }
}


