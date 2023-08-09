package com.pocisoft.eventsapi.data;

import org.hibernate.Session;

import org.hibernate.Transaction;
import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;

import com.pocisoft.eventsapi.data.models.EventDTO;

import java.util.List;
@Repository
public class EventDAO {

	 
    public List<EventSubscription> getSubscriptionsByEvent(int eventId) {
        Session session = HibernateUtil.getSessionFactory().openSession();
        try {
            String hql = "FROM EventSubscription es " +
                    "JOIN FETCH es.event e " +
                    "JOIN FETCH es.userAccount ua " +
                    "WHERE e.id = :eventId";
            Query<EventSubscription> query = session.createQuery(hql, EventSubscription.class);

            query.setParameter("eventId", eventId);
            return query.list();
        } finally {
            session.close();
        }
    }
    public List<Event> getAllEvents() {
    	  return getAllEvents(null);
    	
    }
    public List<Event> getAllEvents(String eventName) {
        Session session = HibernateUtil.getSessionFactory().openSession();
        try {
            String queryString = "SELECT e FROM Event e LEFT JOIN FETCH e.eventSubscriptions es";

            if (eventName != null && !eventName.isEmpty()) {
                queryString += " WHERE e.name LIKE :eventName";
            }

            Query<Event> query = session.createQuery(queryString, Event.class);

            if (eventName != null && !eventName.isEmpty()) {
                query.setParameter("eventName", "%" + eventName + "%");
            }

            List<Event> events = query.list();

            for (Event event : events) {
                event.setCurrentCapacity(event.getMaxCapacity() - event.getEventSubscriptions().size());
            }

            return events;
        } finally {
            session.close();
        }
    }

    public List<Event> getEvents(int userId) {
    	
    	return getEvents(userId,null);
    }
    public List<Event> getEvents(int userId, String eventName) {
        Session session = HibernateUtil.getSessionFactory().openSession();
        try {
            String queryString = "SELECT e FROM Event e LEFT JOIN FETCH e.eventSubscriptions es WHERE e.userId = :userId";

            if (eventName != null && !eventName.isEmpty()) {
                queryString += " AND e.name LIKE :eventName";
            }

            Query<Event> query = session.createQuery(queryString, Event.class);
            query.setParameter("userId", userId);

            if (eventName != null && !eventName.isEmpty()) {
                query.setParameter("eventName", "%" + eventName + "%");
            }

            List<Event> events = query.list();

            for (Event event : events) {
                event.setCurrentCapacity(event.getMaxCapacity() - event.getEventSubscriptions().size());
            }

            return events;
        } finally {
            session.close();
        }
    }

    public Event getEvent(int id) {
        Session session = HibernateUtil.getSessionFactory().openSession();
        try {
        	
        	 Query<Event> query = session.createQuery(
     	            "SELECT e FROM Event e " +
     	            "LEFT JOIN FETCH e.eventSubscriptions es " +
     	            "WHERE e.id = :id",
     	            Event.class
     	        );
            query.setParameter("id", id);
            query.setMaxResults(1);
            return query.uniqueResult();
        } finally {
            session.close();
        }
    }
   
    public Event addEvent(int userId, EventDTO eventDTO) {
        try (Session session = HibernateUtil.getSessionFactory().openSession()) {
            Transaction transaction = session.beginTransaction();

            Event event = new Event();
            event.setName(eventDTO.getName());
            event.setDescription(eventDTO.getDescription());
            event.setMaxCapacity(eventDTO.getMaxCapacity());
            event.setDate(eventDTO.getDate());
            event.setOrganizer(eventDTO.getOrganizer());
            event.setLocation(eventDTO.getLocation());
            event.setStartTime(eventDTO.getStartTime());
            event.setNumberOfHours(eventDTO.getNumberOfHours());
            event.setUserId(userId);

            session.save(event);
            transaction.commit();
            
            return getEvent(event.getId());
        } catch (Exception e) {
            throw new APIException(500, "Failed to add Event: " + e.getMessage());
        }
    }

    public Event updateEvent(int userId,int eventId, EventDTO eventDTO) {
        try (Session session = HibernateUtil.getSessionFactory().openSession()) {
            Transaction transaction = session.beginTransaction();

            Event event = session.get(Event.class, eventId);
            if (event == null) {
                
            }
            if(event.getUserId()!=userId) {
            	throw new APIException(403, "User not allowed to change this event");
            }
            event.setName(eventDTO.getName());
            event.setDescription(eventDTO.getDescription());
            event.setMaxCapacity(eventDTO.getMaxCapacity());
            event.setDate(eventDTO.getDate());
            event.setOrganizer(eventDTO.getOrganizer());
            event.setLocation(eventDTO.getLocation());
            event.setStartTime(eventDTO.getStartTime());
            event.setNumberOfHours(eventDTO.getNumberOfHours());
            

            session.update(event);
            transaction.commit();

            return getEvent(event.getId());
        } catch (APIException e) {
        	throw e;
        }catch (Exception e) {
            throw new APIException(500, "Failed to update event: " + e.getMessage());
        }
    }
    public void deleteEvent(int userId,int eventId) {
        try (Session session = HibernateUtil.getSessionFactory().openSession()) {
            Transaction transaction = session.beginTransaction();

            Event event = session.get(Event.class, eventId);
            if (event == null) {
                throw new APIException(404, "Event not found");
            }
            if(event.getUserId()!=userId) {
            	throw new APIException(403, "User not allowed to delete this event");
            }
            session.delete(event);
            
            transaction.commit();
        } 
        catch (APIException e) {
        	throw e;
        }
        catch (Exception e) {
            throw new APIException(500, "Failed to delete event: " + e.getMessage());
        }
    }
}
