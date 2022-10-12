package example;

import java.io.*;
import java.sql.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Collection;
import javax.servlet.http.*;
import javax.servlet.annotation.*;
import example.api.ApiUtils;

@WebServlet("/hello-servlet")
public class HelloServlet extends HttpServlet {
	private String message;
	// private final String dbhost = "localhost" ;
	private final String dbhost = "postgres-database";
	private final String URL = "jdbc:postgresql://" +dbhost + ":5432/example";
	private final String USER = "postgres";
	private final String PASSWORD = "postgres";
	private final String sql = "select * from \"data\" where id = ?";

	public void init() {
		message = "Hello World!";
		try (Connection connection = connect();
				PreparedStatement pstmt = connection.prepareStatement(sql)) {
			final long id = 1;
			pstmt.setLong(1, id);
			ResultSet rs = pstmt.executeQuery();
			Integer result = null;
			if (rs.next()) {
				result = Integer.parseInt(rs.getString("id"));
				message = "Hello World " + result + "!";
			}
		} catch (SQLException e) {
			e.printStackTrace();
		}

	}

	public Connection connect() throws SQLException {
		try {
			Class.forName("org.postgresql.Driver");
		} catch (ClassNotFoundException eConnection) {
			eConnection.printStackTrace();
		}
		return DriverManager.getConnection(URL, USER, PASSWORD);
	}
	// NOTE: overridden method does not throw java.net.URISyntaxException
	public void doGet(HttpServletRequest request, HttpServletResponse response)
			throws IOException {
		response.setContentType("text/html");
		// NOTE: code copied from another project. 
		// should not throw exception
	        // Integer id = Optional.ofNullable(request.getParameter("id")).map(Integer::parseInt).orElseThrow(() -> new IllegalStateException("detected null id"));
	 	String novalue = "0";
		String value = novalue;
		try {
			Map<String, List<String>> params = ApiUtils.splitQuery(new URI(request.getRequestURI()).getRawQuery());
			value = params.getOrDefault("id", new ArrayList<String>()).stream().findFirst().orElse(novalue);
		} catch (URISyntaxException e){ 
			value = "novalue";
		}
		Integer id = Integer.parseInt(value);
		System.err.println("HelloServlet with id = " + id );
		// Hello
		PrintWriter out = response.getWriter();
		out.println("<html><body>");
		out.println("<h1>" + message + "</h1>");
		out.println("</body></html>");
	}

	public void destroy() {
	}
}

