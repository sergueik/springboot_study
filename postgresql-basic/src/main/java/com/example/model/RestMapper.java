package com.example.model;

import org.springframework.jdbc.core.RowMapper;

import java.sql.ResultSet;
import java.sql.SQLException;

public class RestMapper implements RowMapper<Rest> {
	@Override
	public Rest mapRow(ResultSet rs, int rowNum) throws SQLException {
		Rest rest = new Rest();
		rest.setId(rs.getInt("id"));
		rest.setKey(rs.getString("key"));
		rest.setValue(rs.getString("value"));
		rest.setRand(rs.getInt("rand"));
		return rest;
	}
}