package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Common.IFileStructureConstants;

import java.io.File;
import java.math.BigDecimal;
import java.util.HashMap;
import java.util.Map;

import example.CommandLineParser;

public class Generator {
	private static CommandLineParser commandLineParser;
	private static boolean debug = false;

	@SuppressWarnings("deprecation")
	public static void main(String[] args) throws Exception {

		commandLineParser = new CommandLineParser();

		commandLineParser.saveFlagValue("copybookfile");
		commandLineParser.saveFlagValue("outputfile");

		commandLineParser.saveFlagValue("name");
		commandLineParser.saveFlagValue("accountnumber");
		commandLineParser.saveFlagValue("balance");

		commandLineParser.parse(args);

		String copybookFile = "example.cbl";
		String outputFile = "sample.bin";
		Double balance = 1050.75;
		String name = "JOHN DOE";
		Long accountnumber = 123456789L;

		if (commandLineParser.hasFlag("debug")) {
			debug = true;
		}
		if (debug)
			System.err.println(commandLineParser.getFlags());

		if (commandLineParser.hasFlag("help") || !commandLineParser.hasFlag("copybookfile")
				|| !commandLineParser.hasFlag("outputfile")) {
			System.err.println(String.format("Usage: %s "
					+ "-copybookfile <filename> -outputfile <filename> -name <name> -accountnumber <accountnumber> -balance <balance>\r\n"
					+ "default vlues are name:%s accountnumber = %d balance = %6.2f\r\n", "jar", name, accountnumber,
					balance));
			return;
		}
		if (commandLineParser.hasFlag("outputfile"))
			outputFile = commandLineParser.getFlagValue("outputfile");
		if (commandLineParser.hasFlag("copybookfile"))
			copybookFile = commandLineParser.getFlagValue("copybookfile");

		if (commandLineParser.hasFlag("name"))
			name = commandLineParser.getFlagValue("name").toUpperCase();
		if (commandLineParser.hasFlag("accountnumber"))
			accountnumber = Long.parseLong(commandLineParser.getFlagValue("accountnumber"));
		if (commandLineParser.hasFlag("balance"))
			balance = Double.parseDouble(commandLineParser.getFlagValue("balance"));

		// Create COBOL IO builder
		if (debug)
			System.err.println(String.format("Create COBOL IO builder for %s", copybookFile));
		ICobolIOBuilder builder = JRecordInterface1.COBOL.newIOBuilder(copybookFile)
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH).setFont("cp037"); // EBCDIC

		// Create a new line
		AbstractLine line = builder.newLine();

		// Set fields by name (names must match the copybook)
		line.setField("CUSTOMER-ID", "ABC123");
		line.setField("CUSTOMER-NAME", name);
		line.setField("ACCOUNT-NUMBER", new BigDecimal(accountnumber));
		line.setField("BALANCE", new BigDecimal(balance));

		// Get writer via LineIOProvider using file structure
		AbstractLineWriter writer = LineIOProvider.getInstance().getLineWriter(IFileStructureConstants.IO_FIXED_LENGTH);

		// Write line to FileOutputStream
		writer.open(new java.io.FileOutputStream(outputFile));
		writer.write(line);
		writer.close();

		if (debug)
			System.err.println("EBCDIC row written to: " + outputFile);
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
