package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.CommandLineRunner;
import org.springframework.boot.SpringApplication;
import org.springframework.boot.autoconfigure.SpringBootApplication;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.jdbc.support.rowset.SqlRowSet;

import java.io.PrintStream;
import java.util.List;
import java.util.Scanner;

@SpringBootApplication
public class Application implements CommandLineRunner {

	private static final Logger LOG = LoggerFactory.getLogger(Application.class);
	private final JdbcTemplate jdbcTemplate;

	@Autowired
	public Application(JdbcTemplate jdbcTemplate) {
		this.jdbcTemplate = jdbcTemplate;
	}

	public static void main(String[] args) {
		LOG.info("STARTING THE APPLICATION");
		SpringApplication.run(Application.class, args);
		LOG.info("APPLICATION FINISHED");
	}

	@Override
	public void run(String... args) {
		LOG.info("EXECUTING : command line runner");

		// mvn package && java -jar target/*.jar arg1 arg2
		for (int i = 0; i < args.length; ++i) {
			LOG.info("args[{}]: {}", i, args[i]);
		}

		PrintStream out = System.out;
		Scanner in = new Scanner(System.in);

		// findAll()
		findAll().forEach(out::println);

		out.print("sql? ");
		String sql = in.nextLine();
		out.print("***\n" + query(sql) + "****\n");
	}

	public String query(String sql) {
		StringBuilder response = new StringBuilder();
		SqlRowSet sqlRowSet = jdbcTemplate.queryForRowSet(sql);
		while (sqlRowSet.next()) {
			// NOTE: does not return the query result columns
			response.append(sqlRowSet.getString(1)).append("\n");
		}
		return response.toString();
	}

	public List<Data> findAll() {
		String sql = "select * from \"data\" ";
		return jdbcTemplate.query(sql, Data.getRowMapper());
	}
}
