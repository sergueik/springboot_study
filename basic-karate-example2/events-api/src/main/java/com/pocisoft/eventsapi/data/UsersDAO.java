package com.pocisoft.eventsapi.data;

import org.hibernate.Session;

import org.hibernate.query.Query;
import org.springframework.stereotype.Repository;
@Repository
public class UsersDAO {

    public UsersDAO() {
    }

   
    public UserAccount getUserByUserName(String userName) {
        Session session = HibernateUtil.getSessionFactory().openSession();
        try {
            Query<UserAccount> query = session.createQuery("FROM UserAccount WHERE email = :username", UserAccount.class);
            query.setParameter("username", userName);
            query.setMaxResults(1);
            return query.uniqueResult();
        } catch (Exception e) {
             e.printStackTrace();
             return null;
        }finally {
            session.close();
        }
    }
   
}
