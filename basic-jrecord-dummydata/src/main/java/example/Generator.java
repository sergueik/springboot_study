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
import java.nio.file.Path;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import example.CopybookMetaParser.FieldDef;

@SuppressWarnings("unused")
public class Generator {
	private static boolean debug = false;
	private static boolean parse = false;
	private static Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create();

	@SuppressWarnings("deprecation")
	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);

		String copybookFile = "example.cbl";
		String outputFile = "sample.bin";
		Double balance = 1050.75;
		String name = "JOHN DOE";
		Long accountnumber = 123456789L;

		if (cli.containsKey("debug")) {
			debug = true;
		}
		if (cli.containsKey("parse")) {
			parse = true;
		}
		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("copybookfile") || !cli.containsKey("outputfile")) {
			System.err.println(String.format("Usage: %s "
					+ "-copybookfile <filename> -outputfile <filename> -name <name> -accountnumber <accountnumber> -balance <balance>\r\n"
					+ "default vlues are name:%s accountnumber = %d balance = %6.2f\r\n", "jar", name, accountnumber,
					balance));
			return;
		}
		if (cli.containsKey("outputfile"))
			outputFile = cli.get("outputfile");
		if (cli.containsKey("copybookfile"))
			copybookFile = cli.get("copybookfile");

		if (cli.containsKey("name"))
			name = cli.get("name").toUpperCase();
		if (cli.containsKey("accountnumber"))
			accountnumber = Long.parseLong(cli.get("accountnumber"));
		if (cli.containsKey("balance"))
			balance = Double.parseDouble(cli.get("balance"));
		// Create COBOL IO builder
		if (debug)
			System.err.println(String.format("Create COBOL IO builder for %s", copybookFile));
		ICobolIOBuilder builder = JRecordInterface1.COBOL.newIOBuilder(copybookFile)
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH).setFont("cp037"); // EBCDIC

		// Create a new line
		AbstractLine line = builder.newLine();
		if (debug)
			System.err.println(String.format("Parse %s", copybookFile));

		List<FieldDef> fields = CopybookMetaParser.parse(Path.of(copybookFile));

		if (debug) {
			System.out.println(gson.toJson(fields));
		}
		// future-you (or CI reviewers) will thank you.
		Map<String, Object> populatedValues = new LinkedHashMap<>();

		for (var field : fields) {
			Object value = DummyValueFactory.valueFor(field);
			if (null == value)
				continue;
			try {
				line.setField(field.name, value);
				// NOTE: store what was actually written
				populatedValues.put(field.name, value);
			} catch (Exception e) {
				System.err.println("Unable to set " + field.name + ": " + e.getMessage());
			}
		}
		JsonObject root = new JsonObject();
		JsonArray jsonArray = new JsonArray();

		// one record
		jsonArray.add(gson.toJsonTree(populatedValues));

		root.add("SAMPLE-REC", jsonArray);

		if (debug) {
			System.err.println(gson.toJson(root));
		}
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
