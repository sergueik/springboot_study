package example.utils;

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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonArray;
import com.google.gson.JsonObject;

import example.utils.CopybookMetaParser.FieldDef;

@SuppressWarnings("unused")
public class Generator {

	private final Logger logger = LoggerFactory.getLogger(Generator.class);

	private String copybookFile;
	private String outputFile;
	private Long maxRows;
	private String page;

	private static Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create();

	private boolean debug = true;

	public Generator(String copybookFile, String outputFile, String page, Long maxRows) {
		this.copybookFile = copybookFile;
		this.outputFile = outputFile;
		this.page = page;
		this.maxRows = maxRows;
	}

	public void generate() throws Exception {

		// Create COBOL IO builder
		if (debug)
			logger.info("Create COBOL IO builder for {}", copybookFile);
		ICobolIOBuilder builder = JRecordInterface1.COBOL.newIOBuilder(copybookFile)
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH).setFont(page);
		// Create a new line
		AbstractLine line = builder.newLine();
		if (debug)
			logger.info("Parse {}", copybookFile);

		List<FieldDef> fields = CopybookMetaParser.parse(Path.of(copybookFile));

		if (debug) {
			logger.info("schema: {}", gson.toJson(fields));
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
				logger.error("Unable to set {}: {}", field.name, e.getMessage(), e);
			}
		}
		JsonObject root = new JsonObject();
		JsonArray jsonArray = new JsonArray();

		// one record
		jsonArray.add(gson.toJsonTree(populatedValues));

		root.add("SAMPLE-REC", jsonArray);

		if (debug) {
			logger.info("row: {}", gson.toJson(root));
		}
		// Get writer via LineIOProvider using file structure
		AbstractLineWriter writer = LineIOProvider.getInstance().getLineWriter(IFileStructureConstants.IO_FIXED_LENGTH);

		// Write line to FileOutputStream
		RandomValueFactory randomValueFactory = new RandomValueFactory();
		writer.open(new java.io.FileOutputStream(outputFile));
		for (int i = 0; i < maxRows; i++) {
			line.getFieldValue("ACCOUNT-NUMBER").set(randomValueFactory.randomInt(100000));
			line.getFieldValue("CUSTOMER-NAME").set(randomValueFactory.randomString(10));
			line.getFieldValue("BALANCE").set(randomValueFactory.randomDecimal());
			writer.write(line);
		}
		writer.close();

		if (debug)
			logger.info("EBCDIC row written to: {}", outputFile);
	}
}
