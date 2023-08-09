package com.pocisoft.eventsapi.data;

import org.hibernate.Session;

import org.hibernate.Transaction;
import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;

import com.pocisoft.eventsapi.data.models.EventDTO;

import java.util.Date;
import java.util.List;
@Repository
public class SubscriptionsDAO {

	 
   
    public List<EventSubscription> getSubscriptionsByUser(int userId) {
        Session session = HibernateUtil.getSessionFactory().openSession();
        try {
        	 String hql = "FROM EventSubscription es " +
                     "JOIN FETCH es.event e " +
                     "JOIN FETCH es.userAccount ua " +
                     "WHERE ua.id = :userId";
             Query<EventSubscription> query = session.createQuery(hql, EventSubscription.class);
             query.setParameter("userId", userId);

             return query.list();
        } finally {
            session.close();
        }
    }

    public EventSubscription getSubscription(int subId) {
        Session session = HibernateUtil.getSessionFactory().openSession();
        try {
            String hql = "FROM EventSubscription es " +
                    "JOIN FETCH es.event e " +
                    "JOIN FETCH es.userAccount ua " +
                    "WHERE es.id = :id";
            Query<EventSubscription> query = session.createQuery(hql, EventSubscription.class);

            query.setParameter("id", subId);
            query.setMaxResults(1);
            return query.uniqueResult();
        } finally {
            session.close();
        }
    }
   
    public EventSubscription addSubscription(int userId, int eventId) {
        try (Session session = HibernateUtil.getSessionFactory().openSession()) {
            Transaction transaction = session.beginTransaction();

            Query<Event> eventQuery = session.createQuery("FROM Event where id = :id", Event.class);
            eventQuery.setParameter("id", eventId);
            eventQuery.setMaxResults(1);
            Event event= eventQuery.uniqueResult();
            if(event==null) {
            	 throw new APIException(404, "Failed to add event subscription : Event not found");
            }
            
            Query<Long> countQuery = session.createQuery("SELECT COUNT(*) FROM EventSubscription WHERE eventId = :eventId", Long.class);
            countQuery.setParameter("eventId", eventId);
            Long subscriptionCount = countQuery.uniqueResult();
            
            if(event.getMaxCapacity()<=subscriptionCount)
            {
            	 throw new APIException(400, "Event capacity exceeded");
            }
            
            EventSubscription newSub = new EventSubscription();
            newSub.setUserId(userId);
            newSub.setEventId(eventId);
            newSub.setCreatedAt(new Date());

            session.save(newSub);
            transaction.commit();
            return getSubscription(newSub.getId());
        }
        catch (APIException e) {
        	throw e;
        }
        catch (Exception e) {
            throw new APIException(500, "Failed to add subscription: " + e.getMessage());
        }
    }

   
    public void deleteSubscription(int userId,int subId) {
        try (Session session = HibernateUtil.getSessionFactory().openSession()) {
            Transaction transaction = session.beginTransaction();

            EventSubscription event = session.get(EventSubscription.class, subId);
            if (event == null) {
                throw new APIException(404, "Event not found");
            }
            if(event.getUserId()!=userId) {
            	throw new APIException(403, "User not allowed to delete this subscription");
            }
            session.delete(event);
            
            transaction.commit();
        } 
        catch (APIException e) {
        	throw e;
        }
        catch (Exception e) {
            throw new APIException(500, "Failed to delete subscription: " + e.getMessage());
        }
    }
}
