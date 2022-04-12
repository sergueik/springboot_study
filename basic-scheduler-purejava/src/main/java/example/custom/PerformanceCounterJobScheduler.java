package example.custom;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;

import example.JobScheduler;
import example.custom.YamlConfig;

import java.io.FileInputStream;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class PerformanceCounterJobScheduler {

	private static boolean debug = false;
	private static boolean verbose = false;
	private static String configfile = null;
	private static Integer interval;
	private final static Options options = new Options();
	private static CommandLineParser commandLineparser = new DefaultParser();
	private static CommandLine commandLine = null;
	private Map<String, String> flags = new HashMap<>();
	private static YamlConfig yamlConfig;
	private HelpFormatter helpFormatter = new HelpFormatter();

	public static void main(String[] args) throws Exception {
		options.addOption("h", "help", false, "Help");
		options.addOption(Option.builder("c").longOpt("config").hasArg(true)
				.desc("configuration file").required(true).build());
		options.addOption(Option.builder("d").longOpt("debug").hasArg(false)
				.desc("debug").required(false).build());

		try {
			commandLine = commandLineparser.parse(options, args);

			if (commandLine.hasOption("h")) {
				help();
			}
			if (commandLine.hasOption("d"))
				debug = true;

			if (commandLine.hasOption("config")) {
				configfile = commandLine.getOptionValue("config");
				System.err.println("Loading config file: " + configfile);
				InputStream stream = new FileInputStream(
						System.getProperty("user.dir") + "/" + configfile);
				yamlConfig = YamlConfig.load(stream);
				Map<String, Object> value = yamlConfig.getMap("collect");
				System.err
						.println("Loading config entries for collect:" + value.keySet());

			}

			interval = yamlConfig.getInt("collect.interval");
			verbose = yamlConfig.getBoolean("collect.verbose");
			PerformanceCounterTask collectorTask = new PerformanceCounterTask();
			collectorTask.setVerbose(verbose);
			PerformanceCounterTask computeTask = new PerformanceCounterTask();
			computeTask.setVerbose(true);
			computeTask.setTask(MessageType.COMPUTE);
			JobScheduler jobScheduler = new JobScheduler(0);
			jobScheduler.executeInAndRepeat(collectorTask, interval.intValue(),
					JobScheduler.PER_SECOND);
			jobScheduler.executeInAndRepeat(computeTask, interval * 10,
					30 * JobScheduler.PER_SECOND);
		} catch (ParseException e) {
			System.err.println("Exception parsing command line: " + e.toString());
		}

	}

	public static void help() {
		System.exit(1);
	}

	public static void help(int status) {
		System.exit(status);
	}
}
