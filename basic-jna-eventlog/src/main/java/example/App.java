package example;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.Option;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class App {

	private final static Options options = new Options();
	private final static CommandLineParser commandLineparser = new DefaultParser();
	private static CommandLine commandLine = null;
	public static final int INVALID_OPTION = 42;

	static String name = "EventLog";
	static String server = ".";
	static String source = "example.log4jna_sample";
	// "Application Error";
	static String application = "log4jna_sample";
	// "Application";
	static String resource = "%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll";
	// "%SystemRoot%\\System32\\wer.dll";
	// "%SystemRoot%\\system32\\wevtapi.dll";
	static String message = "the quick brown fox jumps over the lazy dog";
	static int id = 1000;
	private static boolean debug = false;

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
				Arrays.asList(commandLine.getOptions()).stream().forEach((Option o) -> System.err
						.println(String.format("%s (%s): %s", o.getOpt(), o.getLongOpt(), o.getValue())));
				return;
			}
		} catch (ParseException e) {
			System.err.println("Exception parsing command line: " + e.toString());
			System.exit(INVALID_OPTION);
		}
		Win32EventLogAppender appender = Win32EventLogAppender.createAppender(id, name, server, source, application,
				resource, resource);
		appender.append(message);
	}

	public static void help() {
		// @formatter:off
		System.err.println( "Usage:" + "\n"
				+ "java -jar jna_eventlog.jar " 
				+ "-message \"MESSAGE\" " 
				+ "-id EVENTID " 
				+ "-resource \"RESOURCE\" " 
				+ "-application \"APPLICATION\" " 
				+ "-source \"EVENT SOURCE\" " 
				+ "-name \"EVENT LOG NAME\"");
		System.err.println("To write to custom Event Source, use the following argument verbatim:\n" 
				+ "RESOURCE: \"%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll\"\n"
				+ "and create the custom event log using the above as the CategoryMessageFile and EventMessageFile.");
		System.err.println("Example Usage:\n" +
				"java -jar example.jna_eventlog.jar -message \"the quick brown fox jumps over the lazy dog\" -id 12345  -r \"%SystemRoot%\\Microsoft.NET\\Framework\\v4.0.30319\\EventLogMessages.dll\" -application \"log4jna_sample\" -source \"example.log4jna_sample\"  -name \"log4jna_sample\"");
		// @formatter:on
		System.exit(1);
		
	}
}