package example;

import java.io.*;
import java.sql.*;
import java.net.URI;
import java.net.URISyntaxException;
import java.util.ArrayList;
import java.util.Arrays;
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
	private final String URL = "jdbc:postgresql://" + dbhost + ":5432/example";
	private final String USER = "postgres";
	private final String PASSWORD = "postgres";
	private final String sql = "select * from data where id = ?";
	private static final StringBuffer verificationErrors = new StringBuffer();

	public void init() {
		message = "Hello World!";
		final int id = 1;
		List<Integer> ids = new ArrayList<>();
		ids.add(id);
		Long result = queryByIds(ids).get(0);
		message = "Hello World " + result + "!";

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
		// Integer id =
		// Optional.ofNullable(request.getParameter("id")).map(Integer::parseInt).orElseThrow(()
		// -> new IllegalStateException("detected null id"));
		String novalue = "0";
		String value = novalue;
		try {
			Map<String, List<String>> params = ApiUtils
					.splitQuery(new URI(request.getRequestURI()).getRawQuery());
			value = params.getOrDefault("id", new ArrayList<String>()).stream()
					.findFirst().orElse(novalue);
		} catch (URISyntaxException e) {
			value = "novalue";
		}
		Integer id = Integer.parseInt(value);
		Long result = queryById(id);
		System.err.println("HelloServlet with id = " + result);
		// Hello
		PrintWriter out = response.getWriter();
		out.println("<html><body>");
		out.println("<h1>" + message + "</h1>");
		out.println("</body></html>");
	}

	public void destroy() {
	}

	public Long queryById(Integer id) {
		String SQL = "select id from data " + "where id = ?";
		System.err.println("query : " + SQL);
		Long result = null;
		Connection connection = null;
		PreparedStatement pstmt = null;
		ResultSet rs = null;
		try {
			connection = connect();
			pstmt = connection.prepareStatement(SQL);
			pstmt.setInt(1, id);
			rs = pstmt.executeQuery();
			if (rs.next()) {
				result = rs.getLong("id");
				System.err.println("query returned " + result);
			} else {
				System.err.println("query returned nothing ");
			}
		} catch (SQLException e) {
			e.printStackTrace();
		} finally {
			try {
				rs.close();
				pstmt.close();
				connection.close();
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return result;
	}

	public List<Long> queryByIds(List<Integer> ids) {

		int size = ids.size();
		String marks[] = new String[size];
		for (int cnt = 0; cnt != size; cnt++) {
			marks[cnt] = "?";
		}
		String SQL = "select id from data " + String.format("where id in (%s)",
				String.join(",", Arrays.asList(marks)));

		System.err.println("query : " + SQL);
		List<Long> results = new ArrayList<>();
		Connection connection = null;
		PreparedStatement pstmt = null;
		ResultSet rs = null;
		try {
			connection = connect();
			pstmt = connection.prepareStatement(SQL);
			int cnt = 1;
			for (int id : ids) {
				pstmt.setInt(cnt, id);
				cnt++;
			}

			rs = pstmt.executeQuery();
			if (rs.next()) {
				results.add(rs.getLong("id"));
				// TODO: collect all results
			} else {
				System.err.println("query returned nothing ");
			}

		} catch (SQLException e) {
			e.printStackTrace();
		} finally {
			try {
				rs.close();
				pstmt.close();
				connection.close();
			} catch (SQLException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
		return results;
	}

}

