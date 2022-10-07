package example.model;

import org.springframework.jdbc.core.RowMapper;

import java.sql.ResultSet;
import java.sql.SQLException;

public class BackendDataMapper implements RowMapper<BackendData> {
	@Override
	public BackendData mapRow(ResultSet rs, int rowNum) throws SQLException {
		BackendData rest = new BackendData();
		rest.setId(rs.getInt("id"));
		rest.setKey(rs.getString("key"));
		rest.setValue(rs.getString("value"));
		rest.setRand(rs.getInt("rand"));
		return rest;
	}
}