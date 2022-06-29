package example;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Random;
import java.util.concurrent.TimeUnit;

import org.influxdb.InfluxDB;
import org.influxdb.InfluxDBFactory;

import org.influxdb.dto.Point;
import org.influxdb.dto.Pong;
import org.influxdb.dto.Query;
import org.influxdb.dto.QueryResult;
import org.influxdb.dto.QueryResult.Result;
import org.influxdb.dto.QueryResult.Series;
import org.influxdb.dto.BatchPoints;

public class App {

	public static void main(String args[]) throws UnknownHostException {
		String host = "http://192.168.0.29:8086";
		String user = "user";
		String pw = "password";
		String databaseName = "example";
		String seriesName = "testing";

		InfluxDB influxDB = InfluxDBFactory.connect(host, user, pw);
		Pong pong = influxDB.ping();
		System.err.println(pong.getVersion());
		// clearAndCreateDatabase(influxDB, databaseName);
		Random rand = new Random();
		// NOTE: field method is deprecated in favor of addField
		// https://javadoc.io/static/org.influxdb/influxdb-java/2.20/org/influxdb/InfluxDB.html
		// https://www.tabnine.com/code/java/methods/org.influxdb.InfluxDB/write

		influxDB.setDatabase(databaseName);
		Point point = Point.measurement(seriesName).tag("atag", "test")
				.field("idle", 90L).field("usertime", 9L).field("system", 1L).build();

		// org.influxdb.dto.Point.Builder.time(Long timeToSet, TimeUnit precisionToSet)
		point = Point.measurement(seriesName)
				.time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
				.addField("idle", 90L).addField("usertime", 9L).addField("system", 1L)
				.build();
		// NO need to set precision during the write operation: already done in Point builder
		
		influxDB.write(point);
		// NOTE: Enable Gzip compress for http request body
		// is also possile, not tested
		influxDB.enableBatch();
		System.err.println("Starting");
		Collection<Point> points = new HashSet<>();

		for (int i = 0; i < 10; i++) {
			// https://www.tabnine.com/code/java/methods/org.influxdb.dto.Point$Builder/build
			// optional tags
			point = Point.measurement(seriesName)
					.time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
					.tag("host", InetAddress.getLocalHost().getHostName())
					.tag("region", "region").addField("idle", 90L)
					.addField("usertime", 9L).addField("system", 1L).build();
			System.err.println(".");
			points.add(point);
		}
		BatchPoints batchpoints = BatchPoints.builder().points(points).build();
		influxDB.write(batchpoints);
		// NOTE:
		// Exception in thread "main"
		// org.influxdb.InfluxDBException$FieldTypeConflictException: partial
		// write: field type conflict: input field "idle" on measurement "testing"
		// is type integer, already exists as type float dropped=1
		// seeing in Perl Client too
		System.err.println("Done");
		influxDB.flush();
		// https://javadoc.io/static/org.influxdb/influxdb-java/2.17/org/influxdb/InfluxDB.html
		// NOTE: support of streaming queries against a database available,not
		// tested
		influxDB.setLogLevel(InfluxDB.LogLevel.BASIC);

		String queryString = "select * from testing";
		Query query = new Query(queryString, databaseName);
		QueryResult queryResult = influxDB.query(query);
		List<Result> results = queryResult.getResults();
		Iterator<Result> resultsIterator = results.iterator();
		while (resultsIterator.hasNext()) {
			Result result = resultsIterator.next();
			List<Series> listSeries = result.getSeries();
			Iterator<Series> listSeriesIterator = listSeries.iterator();
			while (listSeriesIterator.hasNext()) {
				Series series = listSeriesIterator.next();
				System.err.println("columns: " + series.getColumns());
				System.err.println("values: " + series.getValues());
			}
		}
		influxDB.close();
	}
}
