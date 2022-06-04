package example.utils;

import org.influxdb.InfluxDB;
import org.influxdb.dto.Query;
import org.influxdb.dto.QueryResult;
import org.springframework.stereotype.Component;

@Component
public class InfluxUtil {

	public QueryResult query(String command, String database, InfluxDB influxDB) {
		Query query = new Query(command, database);
		return influxDB.query(query);
	}
}
