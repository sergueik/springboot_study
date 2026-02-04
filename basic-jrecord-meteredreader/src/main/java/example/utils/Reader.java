package example.utils;

import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.databind.JsonSerializer;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.SerializerProvider;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Reader {

	private final static Logger logger = LoggerFactory.getLogger(Reader.class);
	private static boolean debug = true;
	String copybookFile = "example.cbl";
	String inputFile = "example.bin";
	Long maxRows = 1L;
	String page = "cp037";

	private static Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create();

	public Reader(String copybookFile, String inputFile, String page, Long maxRows) {
		this.copybookFile = copybookFile;
		this.inputFile = inputFile;
		this.maxRows = maxRows;
		this.page = page;
	}

	public void parseRecords() throws Exception {

		logger.info("ParseRecords copybook: {}  input: {}  maxRows: {}", copybookFile, inputFile, maxRows);

		ObjectMapper mapper = new ObjectMapper();

		long start = System.currentTimeMillis();
		int count = 0;
		try (CopybookBatchReader reader = new CopybookBatchReader(Path.of(copybookFile), Path.of(inputFile), page)) {

			Map<String, Object> record;
			while ((record = reader.readOne()) != null && count < maxRows) {
				String data = mapper.writeValueAsString(record);

				logger.info(data);
				count++;
			}
		}
		long end = System.currentTimeMillis();
		logger.info("Processed {} records in {} ms", count, (end - start));
	}

}
