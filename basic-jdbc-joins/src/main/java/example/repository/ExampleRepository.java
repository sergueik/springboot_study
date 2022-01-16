package example.repository;

import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.BeanPropertyRowMapper;
import org.springframework.jdbc.core.JdbcTemplate;
import org.springframework.stereotype.Repository;

import example.models.ExampleModel;

@Repository
public class ExampleRepository {

	@Autowired
	private JdbcTemplate template;

	String JOIN_QUERY = "SELECT t1.id, t2.new_id,t1.name,t2.data FROM t1 JOIN t2 ON t1.pattern = t2.pattern";

	public List<ExampleModel> getTestData() {
		return template.query(JOIN_QUERY,
				new BeanPropertyRowMapper<ExampleModel>(ExampleModel.class));
	}

}
