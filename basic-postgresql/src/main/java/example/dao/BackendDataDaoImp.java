package example.dao;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import example.model.BackendData;
import example.model.BackendDataMapper;

import java.util.Arrays;
import java.util.List;
import java.util.Random;

@Transactional
@Repository
public class BackendDataDaoImp implements BackendDataDao {

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

	public List<BackendData> queryByIds(List<Integer> ids) {
		// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/jdbc/core/JdbcTemplate.html
		// query(String sql, Object[] args, ResultSetExtractor<T> rse)
		// Deprecated.
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

		return jdbcTemplate.query(SQL, args, new BackendDataMapper());
	}

	public List<BackendData> queryByIdsAndKeys(List<Integer> ids,
			List<String> keys) {
		// https://docs.spring.io/spring-framework/docs/current/javadoc-api/org/springframework/jdbc/core/JdbcTemplate.html
		// query(String sql, Object[] args, ResultSetExtractor<T> rse)
		// Deprecated.
		int idSize = ids.size();
		int keySize = keys.size();
		Object[] args;
		String idMarks[] = new String[idSize];
		int cnt;
		for (cnt = 0; cnt != idSize; cnt++) {
			idMarks[cnt] = "?";
		}
		args = new Object[idSize + keySize];
		cnt = 0;
		for (cnt = 0; cnt != idSize; cnt++) {
			// for (int id : ids) {
			args[cnt] = ids.get(cnt);
			// cnt++;
		}

		String keyMarks[] = new String[keySize];
		for (cnt = 0; cnt != keySize; cnt++) {
			keyMarks[cnt] = "?";
		}

		cnt = idSize;
		for (cnt = 0; cnt != keySize; cnt++) {
			// for (String key : keys) {
			args[idSize + cnt] = keys.get(cnt);
			// cnt++;
		}

		String SQL = "select id,key,value,rand from rest where "
				+ String.format("id in (%s)", String.join(",", Arrays.asList(idMarks)))
				+ " and " + String.format("key in (%s)",
						String.join(",", Arrays.asList(keyMarks)));

		System.err.println("args " + Arrays.asList(args));
		return jdbcTemplate.query(SQL, args, new BackendDataMapper());
	}
	/*
	 org.springframework.jdbc.BadSqlGrammarException: 
	 PreparedStatementCallback; 
	 bad SQL grammar 
	 [select id,key,value from rest where id in (?,?,?) and key in (?)]; 
	 nested exception is org.postgresql.util.PSQLException: 
	 ERROR: operator does not exist: integer = character varying
	 Hint: No operator matches the given name and argument types. 
	 You might need to add explicit type casts.
	 Position: 40
	 */
}
