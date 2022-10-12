package example.api;

import java.sql.*;

import static java.util.stream.Collectors.groupingBy;
import static java.util.stream.Collectors.mapping;
import static java.util.stream.Collectors.toList;

import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.regex.Pattern;

// NOTE: refactoring in progress
public class QueryUtils {

	// private final String dbhost = "localhost" ;
	private final String dbhost = "postgres-database";
	private final String URL = "jdbc:postgresql://" + dbhost + ":5432/example";
	private final String USER = "postgres";
	private final String PASSWORD = "postgres";

	private QueryUtils() {
	}

	public Connection connect() throws SQLException {
		try {
			Class.forName("org.postgresql.Driver");
		} catch (ClassNotFoundException eConnection) {
			eConnection.printStackTrace();
		}
		return DriverManager.getConnection(URL, USER, PASSWORD);
	}

	public Long queryById(Integer id) {
		String SQL = "select id from data" + "where id = ?";
		Long result = null;
		try (Connection connection = connect();
				PreparedStatement pstmt = connection.prepareStatement(SQL)) {

			pstmt.setInt(1, id);
			ResultSet rs = pstmt.executeQuery();
			if (rs.next())
				result = rs.getLong("id");
		} catch (SQLException e) {
			e.printStackTrace();
		}
		return result;
	}

	public List<Long> queryByIds(List<Integer> ids) {

		int size = ids.size();
		String marks[] = new String[size];
		for (int cnt = 0; cnt != size; cnt++) {
			marks[cnt] = "?";
		}
		String SQL = "select id from \"data\""
				+ String.format("where id in (%s)", Arrays.asList(marks));

		List<Long> results = new ArrayList<>();
		try (Connection connection = connect();
				PreparedStatement pstmt = connection.prepareStatement(SQL)) {
			int cnt = 1;
			for (int id : ids) {
				pstmt.setInt(cnt, id);
				cnt++;
			}

			ResultSet rs = pstmt.executeQuery();
			if (rs.next())
				results.add(rs.getLong("id"));

		} catch (SQLException e) {
			e.printStackTrace();
		}
		return results;
	}
}
