package example;

import net.sf.JRecord.JRecordInterface1;
// import net.sf.JRecord.Common.AbstractLine;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

import java.io.FileOutputStream;
import java.util.HashMap;
import java.util.Map;
/**
 * Copyright 2026 Serguei Kouzmine
 */
import java.io.IOException;
import java.nio.charset.Charset;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;

import example.CommandLineParser;

public class Generator {
	private static boolean debug = false;
	private static String codepage = "CP1047";
	private static CommandLineParser commandLineParser;

	public static void main(String[] args) throws IOException {

		/*
		commandLineParser = new CommandLineParser();

		commandLineParser.saveFlagValue("inputfile");
		commandLineParser.saveFlagValue("data");
		commandLineParser.saveFlagValue("codepage");
		commandLineParser.saveFlagValue("outputfile");
		commandLineParser.saveFlagValue("operation");

		commandLineParser.parse(args);

		if (commandLineParser.hasFlag("debug")) {
			debug = true;
		}

		if (commandLineParser.hasFlag("help")) {
			System.err.println(String.format(
					"Usage: %s -operation=[encode|decode] -data <string> -inputfile <filename> -outputfile <filename> -codepage <codepage>",
					"jar"));
			return;
		}
		
		String data = commandLineParser.getFlagValue("data");
		String outputFile = commandLineParser.getFlagValue("outputfile");
		String inputFile = commandLineParser.getFlagValue("inputfile");
		String operation = commandLineParser.getFlagValue("operation");
		if (commandLineParser.hasFlag("codepage"))
			codepage = commandLineParser.getFlagValue("codepage");

		if (operation == null) {
			System.err.println("Missing required argument: operation");
			return;
		}
*/
		Map<String, String> cli = parseArgs(args);

		String copybook = cli.get("copybook");
		String outFile = cli.get("out");

		if (copybook == null || outFile == null) {
			System.err.println("Required: -copybook <file> -out <file>");
			System.exit(1);
		}

		ICobolIOBuilder builder = JRecordInterface1.COBOL.newIOBuilder(copybook)
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH);

		AbstractLine line = builder.newLine();

		// Optional field overrides
		if (cli.containsKey("customerId")) {
			line.getFieldValue("CUSTOMER-ID").set(cli.get("customerId"));
		}

		if (cli.containsKey("name")) {
			line.getFieldValue("CUSTOMER-NAME").set(cli.get("name"));
		}

		if (cli.containsKey("account")) {
			line.getFieldValue("ACCOUNT-NUMBER").set(Long.parseLong(cli.get("account")));
		}

		if (cli.containsKey("balance")) {
			line.getFieldValue("BALANCE").set(Double.parseDouble(cli.get("balance")));
		}
		// 0.93.3 predates widespread try-with-resources adoption.
			/*
		try (AbstractLineWriter writer = builder.newWriter(new FileOutputStream(outFile))) {
			writer.write(line);
		}
		*/
		AbstractLineWriter writer = null;
		try {
		    writer = builder.newWriter(
		        new FileOutputStream(outFile));
		    writer.write(line);
		} finally {
		    if (writer != null) {
		        writer.close();
		    }
		}

		System.out.println("Written binary record: " + outFile);
		System.out.println("Record length: " + line.getData().length);
	}

	// Extremely simple CLI parser: -key value
	private static Map<String, String> parseArgs(String[] args) {
		Map<String, String> map = new HashMap<>();
		for (int i = 0; i < args.length - 1; i++) {
			if (args[i].startsWith("-")) {
				map.put(args[i].substring(1), args[i + 1]);
				i++;
			}
		}
		return map;
	}
}