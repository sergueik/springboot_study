package example.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import example.model.BackendData;
import example.model.BackendDataMapper;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Random;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

@Transactional
@Repository
public class BackendDataDaoImp implements BackendDataDao {

	private static final Logger logger = LogManager
			.getLogger(BackendDataDaoImp.class);

	@Autowired
	private JdbcTemplate jdbcTemplate;

	@Override
	public List<BackendData> getAll() {
		return jdbcTemplate.query("select * from rest order by id asc",
				new BackendDataMapper());
	}

	@Override
	public BackendData getBackendDataById(int id) {
		return jdbcTemplate.queryForObject("select * from rest where id = ?",
				new Object[] { id }, new BackendDataMapper());
	}

	@Override
	public void addBackendData(BackendData rest) {
		jdbcTemplate.update("INSERT INTO rest (key, value, rand) VALUES (?, ?, ?) ",
				rest.getKey(), rest.getValue(), getRandomNumber());
	}

	@Override
	public void updateBackendData(BackendData rest, int id) {
		jdbcTemplate.update("update rest set key=?, value=? ,rand=? where id=?",
				rest.getKey(), rest.getValue(), getRandomNumber(), id);
	}

	@Override
	public void deleteBackendDataById(int id) {
		jdbcTemplate.update("delete from rest where id=?", id);
	}

	private int getRandomNumber() {
		return (new Random()).nextInt(50);
	}

	@Override
	public int latestInput() {
		return jdbcTemplate.queryForObject(
				"SELECT currval(pg_get_serial_sequence('rest','id'))", Integer.class);
	}

	// NOTE:
	// query(String sql, Object[] args, ResultSetExtractor<T> rse) - Deprecated.
	// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/jdbc/core/JdbcTemplate.html

	public List<BackendData> queryByIds(List<Integer> ids) {
		int size = ids.size();
		String marks[] = new String[size];
		for (int cnt = 0; cnt != size; cnt++) {
			marks[cnt] = "?";
		}
		Object[] args = new Object[size];
		int cnt = 0;
		for (int id : ids) {
			args[cnt] = id;
			cnt++;
		}
		String SQL = "select * from rest where "
				+ String.format("id in (%s)", String.join(",", Arrays.asList(marks)));
		logger.info("sql: " + SQL);
		return jdbcTemplate.query(SQL, args, new BackendDataMapper());
	}

	public List<BackendData> queryByIdsAndKeys(List<Integer> ids,
			List<String> keys) {
		int idSize = ids.size();
		int keySize = keys.size();
		List<Object> args = new ArrayList<>();
		String idMarks[] = new String[idSize];
		Arrays.fill(idMarks, "?");
		args.addAll(ids);

		String keyMarks[] = new String[keySize];
		Arrays.fill(keyMarks, "?");
		args.addAll(idSize, keys);

		String SQL = "select * from rest where "
				+ String.format("id in (%s)", String.join(",", Arrays.asList(idMarks)))
				+ " and " + String.format("key in (%s)",
						String.join(",", Arrays.asList(keyMarks)));

		logger.info("sql: " + SQL);
		logger.info("args: " + args);
		// logged as:
		// args [1, 2, 3, 5, 6, 7, example, example 1]
		// NOTE: if an extra Arrays.asList applied the message format becomes
		// args [[1, 2, 3, 5, 6, 7, example, example 1]]
		return jdbcTemplate.query(SQL, args.toArray(), new BackendDataMapper());
	}
}
