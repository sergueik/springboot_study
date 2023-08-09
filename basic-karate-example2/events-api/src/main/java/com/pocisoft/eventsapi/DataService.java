package com.pocisoft.eventsapi;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.SQLException;
import java.sql.Statement;

import org.h2.tools.Server;

public class DataService {
  
    static {
        try {
            // Register JDBC driver
            Class.forName(AppSettings.JDBC_DRIVER);
        } catch (ClassNotFoundException e) {
            e.printStackTrace();
        }
    }
    

   
    public DataService() {
       
    }
    
   

    public void createTable() {
    	
    	try (Connection connection = openConnection()) {
    		Statement stmt = connection.createStatement();
             String sql = "CREATE TABLE REGISTRATION3 " +
                     "(id INTEGER not NULL, " +
                     "first VARCHAR(255), " +
                     "last VARCHAR(255), " +
                     "age INTEGER, " +
                     "PRIMARY KEY (id))";
             stmt.executeUpdate(sql);
             System.out.println("Created table in the given database...");
    	} catch (SQLException se) {
    	    // Handle exceptions related to the connection or database operations
    	    se.printStackTrace();
    	}
       
    }

    private Connection openConnection() {
        try {
          

        	Connection conn= DriverManager.getConnection(AppSettings.DB_URL, AppSettings.USER, AppSettings.PASS);
        	return conn;

        } catch (Exception e) {
            e.printStackTrace();
            return null;
        }
    }

}
