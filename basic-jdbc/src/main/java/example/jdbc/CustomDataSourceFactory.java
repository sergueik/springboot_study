package example.jdbc;

import java.util.Properties;

import javax.naming.Context;
import javax.sql.DataSource;

import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.apache.tomcat.jdbc.pool.DataSourceFactory;

import java.io.FileInputStream;
import java.io.IOException;
import java.sql.SQLException;
import java.util.Properties;

import javax.sql.DataSource;

// https://github.com/ncredinburgh/secure-tomcat-datasourcefactory/blob/master/src/main/java/com/github/ncredinburgh/tomcat/SecureDataSourceFactory.java
public class CustomDataSourceFactory extends DataSourceFactory {
	private static final Logger logger = LogManager
			.getLogger(DataSourceFactory.class);

	@Override
	public DataSource createDataSource(Properties properties) throws Exception {
		logger.info("Custom createDataSource: {}", properties.getProperty("url"));
		return super.createDataSource(properties);
	}

	@Override
	public DataSource createDataSource(Properties properties, Context context,
			boolean XA) throws Exception {
		logger.info("Custom createDataSource: {}, Context: {}",
				properties.getProperty("url"), context);

		return super.createDataSource(properties, context, XA);
	}
}

