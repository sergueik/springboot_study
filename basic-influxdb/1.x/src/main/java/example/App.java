package example;

import java.net.InetAddress;
import java.net.UnknownHostException;
import java.util.Collection;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.TimeUnit;
import java.lang.IllegalStateException;

import org.influxdb.InfluxDB;
import org.influxdb.InfluxDBFactory;
import org.influxdb.dto.BatchPoints;
import org.influxdb.dto.Point;
import org.influxdb.dto.Pong;
import org.influxdb.dto.Query;
import org.influxdb.dto.QueryResult;
import org.influxdb.dto.QueryResult.Result;
import org.influxdb.dto.QueryResult.Series;

import example.utils.Utils;
import example.utils.PropertiesParser;

public class App {

	private static final String propertiesFileName = "application.properties";
	private static final Map<String, String> propertiesMap = PropertiesParser
			.getProperties(String.format("%s/src/main/resources/%s",
					System.getProperty("user.dir"), propertiesFileName));
	private static final String username = propertiesMap.get("username");
	private static final String password = propertiesMap.get("password");
	private static final String host = propertiesMap.get("host");

	private static String databaseName = "example";
	private static String seriesName = "testing";
	private static InfluxDB influxDB;
	private static Collection<Point> points = new HashSet<>();
	private static Point point;
	private static String metric_hostname;
	private static Random rand = new Random();
	private static String pattern = null;

	private static void testTimestampEpoch() {
		long timestamp;
		String strDate;

		strDate = "Jun 13 2003 23:11:52.454 UTC";
		pattern = "MMM d yyyy HH:mm:ss.SSS zzz";
		timestamp = Utils.dateToEpoch(strDate, pattern);
		System.err
				.println(String.format("date: %s timestamp: %d", strDate, timestamp));
		// 1055545912454
		strDate = "Thu Jun 30 13:52:59 EDT 2022";
		strDate = "Fri Jul 1 13:52:59 EDT 2022";
		// d or dd?
		// dd fails with single digit
		// Exception in thread "main" java.time.format.DateTimeParseException:
		// Text 'Fri Jul 21 13:52:59 EDT 2022' could not be parsed at index 8
		// d does not fail with two digit date
		pattern = "EEE MMM d HH:mm:ss zzz yyyy";
		timestamp = Utils.dateToEpoch(strDate, pattern);
		System.err
				.println(String.format("date: %s timestamp: %d", strDate, timestamp));
		// Exception in thread "main" java.time.format.DateTimeParseException:
		// Text 'Fri Jul 1 13:52:59 EDT 2022' could not be parsed at index 0

		// return;

	}

	private static void importDeviceHistory(InfluxDB influxDB,
			long[] timestamps) {
		influxDB.setDatabase(databaseName);
		if (!influxDB.isBatchEnabled()) {
			try {
				influxDB.enableBatch();
			} catch (IllegalStateException e) {
				System.err.println("Exception (ignored) " + e.toString());
			}
		}
		System.err.println("Starting");
		for (int i = 0; i < timestamps.length; i++) {
			// https://www.tabnine.com/code/java/methods/org.influxdb.dto.Point$Builder/build
			// optional tags
			System.err.println("Adding metric point for " + timestamps[i]);
			point = Point.measurement(seriesName)
					.time(timestamps[i], TimeUnit.MILLISECONDS)
					.tag("host", metric_hostname).tag("region", "region")
					.addField("idle", 10L).addField("usertime", 10L)
					.addField("system", 10L).build();
			System.err.println(".");
			points.add(point);
		}
		BatchPoints batchpoints = BatchPoints.builder().points(points).build();
		influxDB.write(batchpoints);
		System.err.println("Done");
		influxDB.flush();

	}

	private static void importPoint(InfluxDB influxDB) {
		influxDB.setDatabase(databaseName);
		Point point = Point.measurement(seriesName).tag("atag", "test")
				.field("idle", 90.0).field("usertime", 9.0).field("system", 1.0)
				.build();

		// org.influxdb.dto.Point.Builder.time(Long timeToSet, TimeUnit
		// precisionToSet)
		// NOTE: field method is deprecated in favor of addField
		// https://javadoc.io/static/org.influxdb/influxdb-java/2.20/org/influxdb/InfluxDB.html
		// https://www.tabnine.com/code/java/methods/org.influxdb.InfluxDB/write
		point = Point.measurement(seriesName)
				.time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
				.addField("idle", 90.0).addField("usertime", 9.0)
				.addField("system", 1.0).build();
		// NO need to set precision during the write operation: already done in
		// Point builder

		if (influxDB.isBatchEnabled()) {
			try {
				influxDB.disableBatch();
			} catch (IllegalStateException e) {
				System.err.println("Exception (ignored) " + e.toString());
			}
		}
		influxDB.write(point);

	}

	private static void importDeviceEphemeral(InfluxDB influxDB) {

		influxDB.setDatabase(databaseName);
		if (!influxDB.isBatchEnabled()) {
			try {
				influxDB.enableBatch();
			} catch (IllegalStateException e) {

			}
		}
		System.err.println("Starting");
		for (int i = 0; i < 10; i++) {
			// https://www.tabnine.com/code/java/methods/org.influxdb.dto.Point$Builder/build
			// optional tags
			point = Point.measurement(seriesName)
					.time(System.currentTimeMillis(), TimeUnit.MILLISECONDS)
					.tag("host", metric_hostname).tag("region", "region")
					.addField("idle", 40.0).addField("usertime", 19.0)
					.addField("system", 21.0).build();
			points.add(point);
		}
		BatchPoints batchpoints = BatchPoints.builder().points(points).build();
		influxDB.write(batchpoints);
		// write raw data
		final String rawdata = "testing,host=sergueik10,region=region value=60.0,idle=40,usertime=20.0,system=22.0";
		System.err.println("write raw data example: " + rawdata);
		influxDB.write(rawdata);
		// NOTE:
		// Exception in thread "main"
		// org.influxdb.InfluxDBException$FieldTypeConflictException: partial
		// write: field type conflict: input field "idle" on measurement "testing"
		// is type integer, already exists as type float dropped=1
		// seeing in Perl Client too
		System.err.println("Done");
		influxDB.flush();

	}

	private static long[] pastTimestamps = { 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L, 0L,
			0L, 0L };

	public static void generateHistory(long[] pastTimestamps) {
		String strDate = "Fri Jul 1 13:52:59 EDT 2022";
		// add data points in the past
		// date -d "-1 day"
		// NOTE: fragile - prome to
		// Exception in thread "main" java.time.format.DateTimeParseException:
		// Text 'Fri Jul 21 13:52:59 EDT 2022' could not be parsed at index 8
		// d does not fail with two digit date
		// d or dd?
		// dd fails with single digit
		pattern = "EEE MMM d HH:mm:ss zzz yyyy";
		long pastTimestamp = Utils.dateToEpoch(strDate, pattern);
		for (int i = 0; i < pastTimestamps.length; i++) {
			pastTimestamps[i] = pastTimestamp + 100 * i;
		}

	}

	private static void queryData(InfluxDB influxDB, String databaseName,
			String queryString) {
		influxDB.setLogLevel(InfluxDB.LogLevel.BASIC);

		Query query = new Query(queryString, databaseName);
		QueryResult queryResult = influxDB.query(query);
		List<Result> results = queryResult.getResults();
		Iterator<Result> resultsIterator = results.iterator();
		while (resultsIterator.hasNext()) {
			Result result = resultsIterator.next();
			List<Series> listSeries = result.getSeries();
			if (listSeries != null) {
				Iterator<Series> listSeriesIterator = listSeries.iterator();
				if (listSeriesIterator != null) {

					while (listSeriesIterator.hasNext()) {
						Series series = listSeriesIterator.next();
						System.err.println("columns: " + series.getColumns());
						System.err.println("values: " + series.getValues());
					}
				} else {
					System.err.println("No data returned by query: " + queryString);
				}
			} else {
				System.err.println("No data returned by query: " + queryString);
			}
		}

	}

	public static void main(String args[]) throws UnknownHostException {

		testTimestampEpoch();
		metric_hostname = InetAddress.getLocalHost().getHostName();
		System.err
				.println(String.format("connecting host=%s, username=%s, password=%s",
						host, username, password));
		influxDB = InfluxDBFactory.connect(host, username, password);
		Pong pong = influxDB.ping();
		System.err.println(pong.getVersion());
		// clearAndCreateDatabase(influxDB, databaseName);

		importPoint(influxDB);
		// NOTE: May Enable Gzip compress for http request body
		// is also possile, not tested

		importDeviceEphemeral(influxDB);
		generateHistory(pastTimestamps);

		importDeviceHistory(influxDB, pastTimestamps);
		// https://javadoc.io/static/org.influxdb/influxdb-java/2.17/org/influxdb/InfluxDB.html
		// NOTE: support of streaming queries against a database available,not
		// tested

		queryData(influxDB, databaseName,
				"select * from " + seriesName + " where host = 'sergueik10'");
		influxDB.close();

	}
}