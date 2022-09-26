package example.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.simple.SimpleJdbcCall;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;
import java.util.Map;

@Component
public class JdbcTemplateUtils {
	Logger logger = LoggerFactory.getLogger(JdbcTemplateUtils.class);
	private SimpleJdbcCall simpleJdbcCall;

	@Autowired
	@Qualifier("data_mysql")
	public void setDatasource(DataSource datasource) {
		this.simpleJdbcCall = new SimpleJdbcCall(datasource);
	}

	public void callStoreProcedure(String procedureName,
			Map<String, Object> parameters) {
		simpleJdbcCall.withProcedureName(procedureName);
		MapSqlParameterSource inParams = new MapSqlParameterSource();
		if (null != parameters) {
			for (Map.Entry<String, Object> parameter : parameters.entrySet()) {
				inParams.addValue(parameter.getKey(), parameter.getValue());
			}
		}
		simpleJdbcCall.execute(inParams);
		logger.info("PROCEDURE {} IS CALLED", procedureName);
	}

	public Object callStoredFunction(String functionName,
			Map<String, Object> parameters, Class<?> classreturn) {
		simpleJdbcCall.withFunctionName(functionName);
		simpleJdbcCall.withReturnValue();
		MapSqlParameterSource inParams = new MapSqlParameterSource();
		if (null != parameters) {
			for (Map.Entry<String, Object> parameter : parameters.entrySet()) {
				inParams.addValue(parameter.getKey(), parameter.getValue());
			}
		}
		logger.info("FUNCTION {} IS CALLED", functionName);
		return simpleJdbcCall.executeFunction(classreturn, inParams);
	}

}
