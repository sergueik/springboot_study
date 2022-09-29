package example.utils;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Qualifier;
import org.springframework.stereotype.Component;

import javax.sql.DataSource;
import java.sql.CallableStatement;
import java.sql.SQLException;

@Component
public class DatabaseUtils {
	Logger log = LoggerFactory.getLogger(DatabaseUtils.class);
	private final String callFunction = "{ ? = call #statement}";
	private final String callProcedure = "{ call #statement}";
	CallableStatement callableStatement;

	private DataSource dataSource;

	@Autowired
	@Qualifier("data_mysql")
	public void setDataSource(DataSource dataSource) {
		this.dataSource = dataSource;
	}

	public Object callStoredFunction(int sqlReturnType, String functionName,
			Object[] params) {
		try {
			callableStatement = dataSource.getConnection()
					.prepareCall(callFunction.replace("#statement", functionName));

			callableStatement.registerOutParameter(1, sqlReturnType);
			if (params != null) {
				for (int i = 0; i < params.length; i++) {
					callableStatement.setObject((i + 2), params[i]);
				}
			}
			callableStatement.execute();
			log.info("FUNCTION {} is CALLED", functionName);
			return callableStatement.getObject(1);
		} catch (SQLException e) {
			log.error("Error Call Function {} ", functionName, e);
		} finally {
			try {
				if (callableStatement != null) {
					callableStatement.close();
				}
			} catch (Exception e2) {
				log.error("Error Closed Connection ", e2);
			}
		}
		return null;
	}

	public void callStoredProcedure(String procedureName, Object[] params) {
		try {
			callableStatement = dataSource.getConnection()
					.prepareCall(callProcedure.replace("#statement", procedureName));
			if (params != null) {
				for (int i = 0; i < params.length; i++) {
					callableStatement.setObject((i + 1), params[i]);
				}
			}
			callableStatement.execute();
			log.info("PROCEDURE {} is CALLED", procedureName);
		} catch (SQLException e) {
			log.error("Error Call Procedure {} ", procedureName, e);
		} finally {
			try {
				if (callableStatement != null) {
					callableStatement.close();
				}
			} catch (Exception e2) {
				log.error("Error Closed Connection ", e2);
			}
		}
	}

}
