package example;

/**
 * Copyright 2023,2024 Serguei Kouzmine
 */

import java.io.Serializable;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.HelpFormatter;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class App {

	private final static Options options = new Options();
	public static final int INVALID_OPTION = 42;
	private final static CommandLineParser commandLineparser = new DefaultParser();
	private static CommandLine commandLine = null;
	private Map<String, String> flags = new HashMap<>();
	static String name = "EventLog";
	static String server = "."; // guess
	static String source = "Application Error";
	static String application = "Application";
	static String eventMessageFile = "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
	static String categoryMessageFile = "%SystemRoot%\\System32\\wer.dll"; // "%SystemRoot%\\system32\\wevtapi.dll";
	static String resource = "%SystemRoot%\\System32\\wer.dll";
	static String message = "the quick brown fox jumps over the lazy dog";
	static int id = 1000;
	// //

	private static HelpFormatter helpFormatter = new HelpFormatter();
	private static boolean debug = false;

	public void saveFlagValue(String flagName, String longFlagNAme) {
		options.addRequiredOption(flagName, "action", true,
				String.format("%s option", longFlagNAme));
	}

	public static void main(String[] args) {

		options.addOption("h", "help", false, "Help");
		options.addOption("d", "debug", false, "Debug");
		options.addOption("n", "name", true, "Name");
		options.addOption("a", "application", true, "Event Log Application");
		options.addOption("s", "source", true, "Event Log Source");
		options.addOption("i", "id", true, "Event Log Id");
		options.addOption("m", "message", true, "Event Log Message");
		options.addOption("r", "resource", true, "Event Log Category Resource Dll");

		try {
			commandLine = commandLineparser.parse(options, args);
			if (commandLine.hasOption("debug"))
				debug = true;
			if (commandLine.hasOption("h")) {
				help();
			}
			if (commandLine.hasOption("resource")) {
				resource = commandLine.getOptionValue("resource");
			}
			if (commandLine.hasOption("application")) {
				application = commandLine.getOptionValue("application");
			}
			if (commandLine.hasOption("source")) {
				source = commandLine.getOptionValue("source");
			}

			if (commandLine.hasOption("name")) {
				name = commandLine.getOptionValue("name");
			}

			if (commandLine.hasOption("message")) {
				message = commandLine.getOptionValue("message");
			}

			if (commandLine.hasOption("id")) {
				id = Integer.parseInt(commandLine.getOptionValue("id"));
			}

			if (debug) {
				System.err.println("Command line options: ");
				Map<String, String> flags = new HashMap<>();
				Arrays.asList(commandLine.getOptions()).stream().forEach(
						(Option o) -> System.err.println(String.format("%s (%s): %s",
								o.getOpt(), o.getLongOpt(), o.getValue())));
				Arrays.asList(commandLine.getOptions()).stream()
						.forEach(o -> flags.put(o.getArgName(), o.getValue()));
				flags.keySet().stream().forEach(
						o -> System.err.println(String.format("%s", o, flags.get(o))));
				return;
			}
		} catch (ParseException e) {
			System.err.println("Exception parsing command line: " + e.toString());
			System.exit(INVALID_OPTION);
		}
		/*
				StringBuilder b = new StringBuilder();
				for (String str : args) {
					b.append(str);
					b.append(' ');
				}
				String message = b.toString();
		*/

		// "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
		Win32EventLogAppender appender = Win32EventLogAppender.createAppender(id,
				name, server, source, application, resource, resource);
		appender.append(message);
	}

	public static void help() {
		System.exit(1);
	}

}
