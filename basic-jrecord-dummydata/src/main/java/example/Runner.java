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

import example.utils.Generator;

@SuppressWarnings("unused")
public class Runner {
	private static boolean debug = false;
	private static Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create();

	@SuppressWarnings("deprecation")
	public static void main(String[] args) throws Exception {

		Map<String, String> cli = parseArgs(args);

		String copybookFile = "example.cbl";
		String outputFile = "sample.bin";
		String page = "cp037"; // EBCDIC
		Long maxRows = 1L;

		if (cli.containsKey("debug")) {
			debug = true;
		}

		if (debug)
			System.err.println(cli.keySet());

		if (cli.containsKey("help") || !cli.containsKey("copybookfile") || !cli.containsKey("outputfile")) {
			System.err.println(String.format("Usage: jar "
					+ "-copybookfile <filename> -outputfile <filename> -name <name> -accountnumber <accountnumber> -balance <balance>\r\n"));
			return;
		}
		if (cli.containsKey("outputfile"))
			outputFile = cli.get("outputfile");
		if (cli.containsKey("copybookfile"))
			copybookFile = cli.get("copybookfile");
		if (cli.containsKey("maxrows"))
			maxRows = Long.parseLong(cli.get("maxrows"));

		if (cli.containsKey("page"))
			page = cli.get("page");
		// Create COBOL IO builder
		new Generator(outputFile, copybookFile, page, maxRows).generate();
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
