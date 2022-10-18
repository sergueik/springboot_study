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
import java.util.stream.Collectors;

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
	// the filter on the Java side is bandwidth wasteful
	// see the discussion in
	// https://qna.habr.com/q/1210502?e=13146198#clarification_1598732
	// (in Russian)
	/*
	 public List<ContentDB> findAllByTag (final String tag){
	      List<ContentDB>contentDBList = new ArrayList<>();
	
	      Iterable<ContentDB> contentDBS = contentRepository.findAll();
	      contentDBS.forEach(contentDB -> {
	          Set<Tags> tagsSet = contentDB.getTagsSet();
	          tagsSet.stream().forEach(tags -> {
	              if(tags.getTag().equals(tag))
	                  contentDBList.add(contentDB);
	          });
	      });
	     return contentDBList;
	  }
	 */

	// The other option is to use vendor specific features:
	// MySQL: REGEXP LIKE
	// SELECT * FROM rest WHERE key REGEXP '(example 1|example 2|example3)';
	// SQLite - REGEXP, provided the extension is installed first
	// https://stackoverflow.com/questions/5071601/how-do-i-use-regex-in-a-sqlite-query
	// https://dev.mysql.com/doc/refman/8.0/en/regexp.html#operator_regexp
	// MySQL only
	public List<BackendData> queryRegexpOfSetIds(List<Integer> ids) {
		String args[] = new String[1];
		args[0] = String.format("(%s)", String.join("|", ids.stream()
				.map(o -> String.format("%d", o)).collect(Collectors.toList())));
		return jdbcTemplate.query("select * from rest where id REGEXP ?", args,
				new BackendDataMapper());
	}

	public List<BackendData> queryRegexpOfSetKeys(List<String> keys) {
		String args[] = new String[1];
		args[0] = String.format("(%s)", String.join("|", keys));
		// NOTE: have to use quotes ` with MySQL JDBC template string:
		// cannot have bare column named "key" without quoting
		// would get an exception in runtime :
		// with root cause
		// java.sql.SQLSyntaxErrorException:
		// You have an error in your SQL syntax;
		// check the manual that corresponds to your MySQL server version
		// for the right syntax to use near
		// 'key REGEXP '(example 1|example 2|example 3)''
		return jdbcTemplate.query("select * from rest where `key` REGEXP ?", args,
				new BackendDataMapper());
	}

	// PostgreSQL SIMILAR TO
	// SELECT * FROM rest WHERE key SIMILAR TO '(example 1|example 2|example3)';
	// https://www.postgresql.org/docs/current/functions-matching.html
	// PostgreSQL only
	public List<BackendData> querySimilarToSetIds(List<Integer> ids) {
		String args[] = new String[1];
		args[0] = String.format("(%s)", String.join("|", ids.stream()
				.map(o -> String.format("%d", o)).collect(Collectors.toList())));
		return jdbcTemplate.query("select * from rest where id SIMILAR TO ?", args,
				new BackendDataMapper());
	}

	public List<BackendData> querySimilarToSetKeys(List<String> keys) {
		String args[] = new String[1];
		args[0] = String.format("(%s)", String.join("|", keys));
		// NOTE: cannot use ` within PostgreSQL JDBC template string:
		// org.postgresql.util.PSQLException: ERROR: operator does not exist: `
		// character varying
		return jdbcTemplate.query("select * from rest where key SIMILAR TO ?", args,
				new BackendDataMapper());
	}
}

