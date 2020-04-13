package com.example.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import com.example.model.Rest;
import com.example.model.RestMapper;

import java.util.List;
import java.util.Random;

@Transactional
@Repository
public class RestDaoImp implements RestDao {

	@Autowired
	private JdbcTemplate jdbcTemplate;

	@Override
	public List<Rest> getAll() {
		return jdbcTemplate.query("select * from rest order by id asc", new RestMapper());
	}

	@Override
	public Rest getRestById(int id) {
		return jdbcTemplate.queryForObject("select * from rest where id = ?", new Object[] { id }, new RestMapper());
	}

	@Override
	public void addRest(Rest rest) {
		jdbcTemplate.update("INSERT INTO rest (key, value, rand) VALUES (?, ?, ?) ", rest.getKey(), rest.getValue(),
				getRandomNumber());
	}

	@Override
	public void updateRest(Rest rest, int id) {
		jdbcTemplate.update("update rest set key=?, value=? ,rand=? where id=?", rest.getKey(), rest.getValue(),
				getRandomNumber(), id);
	}

	@Override
	public void deleteRestById(int id) {
		jdbcTemplate.update("delete from rest where id=?", id);
	}

	private int getRandomNumber() {
		return (new Random()).nextInt(50);
	}

	@Override
	public int latestInput() {
		return jdbcTemplate.queryForObject("SELECT currval(pg_get_serial_sequence('rest','id'))", Integer.class);
	}
}