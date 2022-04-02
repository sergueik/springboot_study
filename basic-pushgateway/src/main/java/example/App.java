package example;

import java.util.concurrent.atomic.AtomicInteger;

/**
 * Copyright 2021 Serguei Kouzmine
 */

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

//
public class App {

	public String getName() {
		return name;
	}

	public void setName(String value) {
		if (value != null)
			name = value;
	}

	protected static String osName = getOSName();
	private AtomicInteger infoGaugeValue = new AtomicInteger(1);
	private String name = "batchjob";
	private static boolean debug = false;
	private final static Options options = new Options();
	private static CommandLineParser commandLineparser = new DefaultParser();
	private static CommandLine commandLine = null;
	private static Boolean status = true;
	private static int interval = 3000;
	private String gateway = "127.0.0.1:9091";

	public String getGateway() {
		return gateway;
	}

	public void setGateway(String value) {
		if (value != null)
			gateway = value;
	}

	void executeBatchJob(Boolean status) throws Exception {
		CollectorRegistry collectorRegistry = new CollectorRegistry();

		if (debug) {
			System.err
					.println("get CollectorRegistry: " + collectorRegistry.hashCode());
		}

		Gauge duration = Gauge.build().name("job_duration_seconds")
				.help("Job duration in seconds.").register(collectorRegistry);
		Gauge.Timer durationTimer = duration.startTimer();
		if (debug) {
			System.err.println("Set job duration timer: " + durationTimer.hashCode());
		}
		try {
			Gauge jobInfo = Gauge.build().name("job_info").help("Job identifier.")
					.labelNames("test_suite", "test_name", "more_info")
					.register(collectorRegistry);
			jobInfo.labels("test suite", "test name", "more information");
			if (debug) {
				System.err.println("Set job info gauge: " + jobInfo.hashCode() + " "
						+ jobInfo.describe());
			}
			try {
				// jobInfo.inc();
				// https://github.com/prometheus/client_java/blob/parent-0.10.0/simpleclient/src/main/java/io/prometheus/client/Gauge.java#L247
				jobInfo.set((double) 1);
				// https://github.com/prometheus/client_java/blob/parent-0.10.0/simpleclient/src/main/java/io/prometheus/client/Gauge.java#L265
			} catch (NullPointerException e) {
				// ignore
				
				System.err.println("Exception (ignored): " + e.toString());
				e.printStackTrace();
			}

			if (debug) {
				System.err.println(
						"Executing job with status: " + (status ? "success" : "failure"));
			}
			Thread.sleep(interval);
			if (!status) {
				// https://www.baeldung.com/java-new-custom-exception
				if (debug) {
					System.err.println("Job is failing");
				}
				throw new JobException("Job is failing");
			}
			if (debug) {
				System.err.println("Job complete");
			}
			// This is only added to the registry after success,
			// so that a previous success in the Pushgateway is not overwritten on
			// failure.
			Gauge lastSuccess = Gauge.build().name("job_last_success")
					.help("Last successful job run").register(collectorRegistry);
			lastSuccess.setToCurrentTime();
			if (debug) {
				System.err.println("Set job success gauge: " + lastSuccess.describe());
			}
		} catch (JobException e) {
			if (debug) {
				System.err.println("Job Failure processing block");
			}
			// NOTE: adding labels to every operational metric is not recommended
			// instead join the "info" gauge metric with operational using PROQL
			Gauge lastFailure = Gauge.build().name("job_last_failure")
					.help("Last failed job run").register(collectorRegistry);
			lastFailure.setToCurrentTime();
			if (debug) {
				System.err.println("Set job failure gauge: " + lastFailure.describe());
			}
		} finally {
			durationTimer.setDuration();
			if (debug) {
				System.err.println("Sending job info");
			}
			new PushGateway(gateway).pushAdd(collectorRegistry, name);
		}
	}

	public static void main(String[] args) {
		String name = null;

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
			if (debug) {
				System.err.println("Executing job: " + app.getName() + " with status: "
						+ (status ? "success" : "failure"));
			}
			try {
				app.executeBatchJob(status);
			} catch (Exception e) {
				System.err.println("Unexpected exception: " + e.toString());
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
