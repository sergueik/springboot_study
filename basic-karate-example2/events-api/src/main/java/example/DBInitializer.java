package example;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.PreparedStatement;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Statement;
import java.util.List;
import org.h2.tools.Server;

import com.fasterxml.jackson.core.JsonParseException;
import com.fasterxml.jackson.core.type.TypeReference;
import com.fasterxml.jackson.databind.DeserializationFeature;
import com.fasterxml.jackson.databind.JsonMappingException;
import com.fasterxml.jackson.databind.ObjectMapper;
import java.io.IOException;
import java.io.InputStream;

public class DBInitializer {

	static {
		try {

			// Register JDBC driver
			Class.forName(AppSettings.JDBC_DRIVER);
		} catch (ClassNotFoundException e) {
			e.printStackTrace();
		}
	}

	public static void initializeH2DBServer() {
		try {
			Server server = Server.createTcpServer("-ifNotExists", "-tcpAllowOthers")
					.start();
			Server webServer = Server.createWebServer("-ifNotExists").start();

			String dbUrl = server.getURL();
			String webServerURL = webServer.getURL();

			System.out.println("H2 web server started: " + webServerURL);
			System.out.println("H2 db server started: " + dbUrl);

			CreateInitialData();

		} catch (SQLException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	public static void CreateInitialData() {
		InitializeUsersIfNecessary();
		InitializeEventsIfNecessary();
		InitializeEventSubscriptionsIfNecessary();

	}

	private static void InitializeEventsIfNecessary() {
		try (Connection conn = openConnection()) {

			String tableName = "events";

			if (tableExistsSQL(conn, tableName)) {
				System.out.println(
						"Table exists: " + tableName + ". Skipped data initialization");
				return;
			}

			Statement stmt = conn.createStatement();
			stmt.executeUpdate("CREATE TABLE " + tableName + " (\r\n"
					+ "  id INT AUTO_INCREMENT PRIMARY KEY,\r\n"
					+ "  name NVARCHAR(250) NOT NULL,\r\n"
					+ "  description NVARCHAR(MAX),\r\n"
					+ "  maxCapacity INT NOT NULL,\r\n" + "  date DATE NOT NULL,\r\n"
					+ "  organizer NVARCHAR(250) NOT NULL,\r\n"
					+ "  location NVARCHAR(250) NOT NULL,\r\n"
					+ "  startTime NVARCHAR(50) NOT NULL,\r\n"
					+ "  numberOfHours INT NOT NULL,\r\n" + "  userId INT NOT NULL,\r\n"
					+ "  FOREIGN KEY (userId) REFERENCES UserAccounts(id)\r\n" + ");\r\n"
					+ "");
			System.out.println("Created table " + tableName + "...");
			//
			ObjectMapper objectMapper = new ObjectMapper();
			objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES,
					false);

			InputStream jsonStream = DBInitializer.class.getClassLoader()
					.getResourceAsStream("data/events.json");

			List<EventEntity> events = objectMapper.readValue(jsonStream,
					new TypeReference<List<EventEntity>>() {
					});

			String sql = "INSERT INTO events (name, description, maxCapacity, date, organizer, location, startTime, numberOfHours, userId) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)";
			PreparedStatement statement = conn.prepareStatement(sql);

			for (EventEntity event : events) {
				statement.setString(1, event.getName());
				statement.setString(2, event.getDescription());
				statement.setInt(3, event.getMaxCapacity());
				statement.setString(4, event.getDate());
				statement.setString(5, event.getOrganizer());
				statement.setString(6, event.getLocation());
				statement.setString(7, event.getStartTime());
				statement.setInt(8, event.getNumberOfHours());
				statement.setInt(9, event.getUserId());

				statement.addBatch();
			}

			statement.executeBatch();
			System.out
					.println(tableName.toUpperCase() + " Data inserted successfully.");
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (JsonParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JsonMappingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static void InitializeEventSubscriptionsIfNecessary() {
		try (Connection conn = openConnection()) {

			String tableName = "eventsubscriptions";

			if (tableExistsSQL(conn, tableName)) {
				System.out.println(
						"Table exists: " + tableName + ". Skipped data initialization");
				return;
			}

			Statement stmt = conn.createStatement();
			stmt.executeUpdate("CREATE TABLE " + tableName + " (\r\n"
					+ "    id INT AUTO_INCREMENT PRIMARY KEY,\r\n"
					+ "    eventId INT NOT NULL,\r\n" + "    userId INT NOT NULL,\r\n"
					+ "    createdAt DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,\r\n"
					+ "    FOREIGN KEY (eventId) REFERENCES Events(id) ON DELETE CASCADE,\r\n"
					+ "    FOREIGN KEY (userId) REFERENCES UserAccounts(id) ON DELETE CASCADE\r\n"
					+ ");");
			System.out.println("Created table " + tableName + "...");
			//
			ObjectMapper objectMapper = new ObjectMapper();
			objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES,
					false);

			InputStream jsonStream = DBInitializer.class.getClassLoader()
					.getResourceAsStream("data/eventsubscriptions.json");

			List<EventSubscriptionEntity> eventSubscriptions = objectMapper.readValue(
					jsonStream, new TypeReference<List<EventSubscriptionEntity>>() {
					});

			String sql = "INSERT INTO " + tableName
					+ " (eventId, userId) VALUES (?, ?)";
			PreparedStatement statement = conn.prepareStatement(sql);

			for (EventSubscriptionEntity event : eventSubscriptions) {
				statement.setInt(1, event.getEventId());
				statement.setInt(2, event.getUserId());
				statement.addBatch();
			}

			statement.executeBatch();
			System.out
					.println(tableName.toUpperCase() + " Data inserted successfully.");
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (JsonParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JsonMappingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static void InitializeUsersIfNecessary() {
		try (Connection conn = openConnection()) {

			String tableName = "useraccounts";
			InputStream jsonStream = DBInitializer.class.getClassLoader()
					.getResourceAsStream("data/users.json");

			if (tableExistsSQL(conn, tableName)) {
				System.out.println(
						"Table exists: " + tableName + ". Skipped data initialization");
				return;
			}

			Statement stmt = conn.createStatement();
			stmt.executeUpdate(
					"CREATE TABLE " + tableName + " (\r\n" + "  id INT  PRIMARY KEY,\r\n"
							+ "  firstname NVARCHAR(250) NOT NULL,\r\n"
							+ "  lastname NVARCHAR(250) NOT NULL,\r\n"
							+ "  email NVARCHAR(250) UNIQUE NOT NULL,\r\n"
							+ "  roles NVARCHAR(MAX),\r\n"
							+ "  password NVARCHAR(MAX) NOT NULL\r\n" + ")");
			System.out.println("Created table " + tableName + "...");

			ObjectMapper objectMapper = new ObjectMapper();
			objectMapper.configure(DeserializationFeature.FAIL_ON_UNKNOWN_PROPERTIES,
					false);

			List<UserEntity> users = objectMapper.readValue(jsonStream,
					new TypeReference<List<UserEntity>>() {
					});

			String sql = "INSERT INTO " + tableName
					+ " (id,firstname,lastname,email, password,roles) VALUES (?, ?, ?, ?, ?, ?)";
			PreparedStatement statement = conn.prepareStatement(sql);

			for (UserEntity user : users) {

				statement.setInt(1, user.getId());
				statement.setString(2, user.getFirstName());
				statement.setString(3, user.getLastName());
				statement.setString(4, user.getEmail());
				statement.setString(5, user.getPassword());
				statement.setString(6, user.getRoles());
				statement.addBatch();
			}

			statement.executeBatch();
			System.out
					.println(tableName.toUpperCase() + " Data inserted successfully.");
		} catch (SQLException e) {
			e.printStackTrace();
		} catch (JsonParseException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (JsonMappingException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (IOException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
	}

	private static boolean tableExistsSQL(Connection connection, String tableName)
			throws SQLException {
		PreparedStatement preparedStatement = connection
				.prepareStatement("SELECT count(*) " + "FROM information_schema.tables "
						+ "WHERE table_name = ?" + "LIMIT 1;");
		preparedStatement.setString(1, tableName.toUpperCase());

		ResultSet resultSet = preparedStatement.executeQuery();
		resultSet.next();
		return resultSet.getInt(1) != 0;
	}

	private static Connection openConnection() {
		try {

			Connection conn = DriverManager.getConnection(AppSettings.DB_URL,
					AppSettings.USER, AppSettings.PASS);
			return conn;

		} catch (Exception e) {
			e.printStackTrace();
			return null;
		}
	}

}

class EventSubscriptionEntity {
	private int eventId;
	private int userId;

	public int getEventId() {
		return eventId;
	}

	public void setEventId(int eventId) {
		this.eventId = eventId;
	}

	public int getUserId() {
		return userId;
	}

	public void setUserId(int userId) {
		this.userId = userId;
	}
}

class EventEntity {
	private String name;
	private String description;
	private int maxCapacity;
	private String date;
	private String organizer;
	private String location;
	private String startTime;
	private int numberOfHours;
	private int userId;

	// Getters and setters...

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public int getMaxCapacity() {
		return maxCapacity;
	}

	public void setMaxCapacity(int maxCapacity) {
		this.maxCapacity = maxCapacity;
	}

	public String getDate() {
		return date;
	}

	public void setDate(String date) {
		this.date = date;
	}

	public String getOrganizer() {
		return organizer;
	}

	public void setOrganizer(String organizer) {
		this.organizer = organizer;
	}

	public String getLocation() {
		return location;
	}

	public void setLocation(String location) {
		this.location = location;
	}

	public String getStartTime() {
		return startTime;
	}

	public void setStartTime(String startTime) {
		this.startTime = startTime;
	}

	public int getNumberOfHours() {
		return numberOfHours;
	}

	public void setNumberOfHours(int numberOfHours) {
		this.numberOfHours = numberOfHours;
	}

	public int getUserId() {
		return userId;
	}

	public void setUserId(int userId) {
		this.userId = userId;
	}
}

class UserEntity {
	private String email;
	private String password;
	private String roles;
	private String firstName;
	private String lastName;
	private int id;

	public String getEmail() {
		return email;
	}

	public void setEmail(String email) {
		this.email = email;
	}

	public String getPassword() {
		return password;
	}

	public void setPassword(String password) {
		this.password = password;
	}

	public String getRoles() {
		return roles;
	}

	public void setRoles(String roles) {
		this.roles = roles;
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

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}
}
