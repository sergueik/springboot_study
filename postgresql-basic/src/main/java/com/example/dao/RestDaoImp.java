package com.example.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.example.model.Rest;
import com.example.model.RestMapper;
import com.example.model.RestResult;

import java.util.List;
import java.util.Random;

@Transactional
@Repository
public class RestDaoImp implements RestDao {
	@Autowired
	private JdbcTemplate jdbcTemplate;

	@Override
	public List<RestResult> getAll() {
		String sql = "select * from rest order by id asc";
		List<RestResult> rest = jdbcTemplate.query(sql, new RestMapper());
		return rest;
	}

	@Override
	public RestResult getRestById(int id) {
		String sql2 = "select * from rest where id = ?";
		RestResult rest2 = jdbcTemplate.queryForObject(sql2, new Object[] { id },
				new RestMapper());
		return rest2;
	}

	@Override
	public void addRest(Rest rest) {
		String sql = "INSERT INTO rest (key, value, rand) VALUES (?, ?, ?) ";
		jdbcTemplate.update(sql, rest.getKey(), rest.getValue(), getRandomNumber());
	}

	@Override
	public void updateRest(Rest rest, int id) {
		int a = getRandomNumber();
		String sql = "update rest set key=?, value=? ,rand=? where id=?";
		jdbcTemplate.update(sql, rest.getKey(), rest.getValue(), a, id);

	}

	@Override
	public void deleteRestById(int id) {
		String sql = "delete from rest where id=?";
		jdbcTemplate.update(sql, id);
	}

	private int getRandomNumber() {
		Random rand = new Random();
		return rand.nextInt(50);
	}

	@Override
	public int latestInput() {
		String sql2 = "SELECT currval(pg_get_serial_sequence('rest','id'))";
		int id = jdbcTemplate.queryForObject(sql2, Integer.class);
		return id;
	}
}