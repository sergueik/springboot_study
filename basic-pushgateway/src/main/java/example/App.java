package example;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

import io.prometheus.client.CollectorRegistry;
import io.prometheus.client.Gauge;
import io.prometheus.client.exporter.PushGateway;

public class App {

	public String getName() {
		return name;
	}

	public void setName(String value) {
		if (value != null)
			name = value;
	}

	protected static String osName = getOSName();
	public static final int INVALID_OPTION = 42;
	private String name = "test_pushgateway_job";
	private static boolean debug = false;
	private final static Options options = new Options();
	private static CommandLineParser commandLineparser = new DefaultParser();
	private static CommandLine commandLine = null;
	private static Boolean status = true;
	private static int interval = 3000;
	private String gateway = "127.0.0.1:9091";
	private PushGateway pg = new PushGateway(gateway);

	public String getGateway() {
		return gateway;
	}

	public void setGateway(String value) {
		if (value != null)
			gateway = value;
	}

	void executeBatchJob(Boolean status) throws Exception {
		CollectorRegistry registry = new CollectorRegistry();
		Gauge duration = Gauge.build().name("test_pushgateway_job_duration_seconds")
				.help("Job duration in seconds.").register(registry);
		Gauge.Timer durationTimer = duration.startTimer();
		try {

			System.err.println(
					"Executing job with status " + (status ? "success" : "failure"));
			Thread.sleep(interval);
			if (!status) {
				// https://www.baeldung.com/java-new-custom-exception
				throw new JobException("exception has occured");
			}

			System.err.println("Job Complete");
			// This is only added to the registry after success,
			// so that a previous success in the Pushgateway is not overwritten on
			// failure.
			Gauge lastSuccess = Gauge.build()
					.name("test_pushgateway_job_last_success_unixtime")
					.help("Last successful job run time").register(registry);
			lastSuccess.setToCurrentTime();
		} catch (JobException e) {
			System.err.println("Job Exception handling block");
			Gauge lastFailure = Gauge.build()
					.name("test_pushgateway_job_last_failure_unixtime")
					.help("Last failed job run time").register(registry);
			lastFailure.setToCurrentTime();
		} finally {
			durationTimer.setDuration();
			pg = new PushGateway(gateway);
			pg.pushAdd(registry, name);
		}
	}

	public static void main(String[] args) {

		options.addOption("h", "help", false, "help");
		options.addOption("d", "debug", false, "debug");
		options.addOption("n", "name", true, "name");
		options.addOption("g", "gateway", true, "gateway");
		options.addOption("s", "status", true, "status");
		options.addOption("w", "interval", true, "run interval");
		try {
			commandLine = commandLineparser.parse(options, args);
			if (commandLine.hasOption("h")) {
				help();
			}
			if (commandLine.hasOption("d")) {
				debug = true;
			}
			String arg = commandLine.getOptionValue("status");
			if (arg == null) {

				System.err.println(
						"Missing or invalid boolean argument: status, using defult");
				status = true;
			} else {
				status = Boolean.parseBoolean(arg);
			}
			arg = commandLine.getOptionValue("interval");
			if (arg == null) {
				System.err.println("Missing argument: delay, using defult");
			} else {
				interval = Integer.parseInt(arg);
			}
			String name = null;
			if (commandLine.hasOption("name")) {
				name = commandLine.getOptionValue("name");
			}
			String gateway = null;
			if (commandLine.hasOption("gateway")) {
				gateway = commandLine.getOptionValue("gateway");
			}

			App app = new App();
			app.setName(name);
			app.setGateway(gateway);
			System.err.println("Starting Job " + app.getName());
			try {
				app.executeBatchJob(status);
			} catch (Exception e) {
				System.err.println("Unexpected failure");
			}
		} catch (ParseException e) {
		}

	}

	public static String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}

	public static void help() {
		System.exit(1);
	}

	@SuppressWarnings("serial")
	public class JobException extends Exception {
		public JobException(String message) {
			super(message);
		}
	}
}
