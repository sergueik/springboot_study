package example;

import java.time.Instant;
import java.util.List;
import java.util.Map;

import com.influxdb.annotations.Column;
import com.influxdb.annotations.Measurement;
import com.influxdb.client.InfluxDBClient;
import com.influxdb.client.InfluxDBClientFactory;
import com.influxdb.client.QueryApi;
import com.influxdb.client.WriteApiBlocking;
import com.influxdb.client.domain.WritePrecision;
import com.influxdb.client.write.Point;
import com.influxdb.query.FluxColumn;
import com.influxdb.query.FluxRecord;
import com.influxdb.query.FluxTable;

import example.utils.PropertiesParser;

// based on: https://github.com/influxdata/influxdb-client-java

public class App {

	private static final boolean debug = false;
	private static final String propertiesFileName = "application.properties";
	private static final Map<String, String> propertiesMap = PropertiesParser
			.getProperties(String.format("%s/src/main/resources/%s",
					System.getProperty("user.dir"), propertiesFileName));
	private static final String host = propertiesMap.get("host");
	private static final char[] token = propertiesMap.get("token").toCharArray();;
	private static String org = propertiesMap.get("org");
	private static String bucket = propertiesMap.get("bucket");

	public static void main(final String[] args) {

		InfluxDBClient influxDBClient = InfluxDBClientFactory.create(host, token,
				org, bucket);

		WriteApiBlocking writeApi = influxDBClient.getWriteApiBlocking();

		Point point = Point.measurement("temperature").addTag("location", "west")
				.addField("value", 55D)
				.time(Instant.now().toEpochMilli(), WritePrecision.MS);

		System.err.println("Writing Point instance");
		writeApi.writePoint(point);

		System.err.println("Writing raw metric payload");
		writeApi.writeRecord(WritePrecision.NS,
				"temperature,location=north value=60.0");

		Temperature temperature = new Temperature();
		temperature.location = "south";
		temperature.value = 62D;
		temperature.time = Instant.now();

		System.err.println("Writing custom pojo");
		writeApi.writeMeasurement(WritePrecision.NS, temperature);

		String fluxQuery = String.format(
				"from(bucket:\"%s\") " + "|> range(start: 0)"
						+ "|> filter(fn: (r) => r[\"_measurement\"] == \"temperature\")",
				bucket);

		// NOTE: "filter" should be placed *after* "range" in the query
		// composing query incorrectly leads to an error:
		// Exception in thread "main" com.influxdb.exceptions.BadRequestException:
		// HTTP status code: 400; Message: error in building plan while starting
		// program: cannot submit unbounded read to "testbucket"; try bounding
		// 'from' with a call to 'range'

		System.err.println(
				String.format("Querying the data via Flux: \"%s\"", fluxQuery));
		QueryApi queryApi = influxDBClient.getQueryApi();

		List<FluxTable> tables = queryApi.query(fluxQuery);
		System.err.println(String.format("Received %d tables", tables.size()));
		for (FluxTable fluxTable : tables) {
			System.err.println("Processing table: " + fluxTable.toString());

			List<FluxColumn> columns = fluxTable.getColumns();
			if (debug)
				System.err.println("Processing table columns: " + columns.toString());
			// TODO: figure how to filter the result by the "_measurement" column
			// to review the data just inserted
			/* if (columns.contains("temperature")) {
			
			}
			*/
			List<FluxRecord> records = fluxTable.getRecords();
			System.err.println(String.format("Received %d records in table %s",
					records.size(), fluxTable));

			for (FluxRecord fluxRecord : records) {
				// can also use fluxRecord.getValueByKey("_measurement"),
				// fluxRecord.getValueByKey("_value")
				System.err.println("time: " + fluxRecord.getTime() + "\t" + "value: "
						+ fluxRecord.getValue() + "\t" + "measurement: "
						+ fluxRecord.getValueByKey("_measurement") + "\t" + "location: "
						+ fluxRecord.getValues().get("location"));
			}

		}
		influxDBClient.close();
	}

	@Measurement(name = "temperature")
	private static class Temperature {

		@Column(tag = true)
		String location;

		@Column
		Double value;

		@Column(timestamp = true)
		Instant time;
	}
}