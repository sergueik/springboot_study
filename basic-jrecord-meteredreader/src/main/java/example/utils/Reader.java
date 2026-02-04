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

public class Reader {

	private static boolean debug = true;
	String copybookFile = "example.cbl";
	String inputFile = "example.bin";
	Long maxRows = 1L;
	String page = "cp037";

	private static Gson gson = new GsonBuilder().setPrettyPrinting().serializeNulls().create();

	public Reader(String copybookFile, String inputFile, String page, Long maxRows) {
		super();
		this.copybookFile = copybookFile;
		this.inputFile = inputFile;
		this.maxRows = maxRows;
		this.page = page;
	}

	public void parseRecords() throws Exception {

		System.err.println("ParseRecords copybook:" + copybookFile + " input:" + inputFile + " maxRows: " + maxRows);

		ObjectMapper mapper = new ObjectMapper();

		long start = System.currentTimeMillis();
		int count = 0;
		try (CopybookBatchReader reader = new CopybookBatchReader(Path.of(copybookFile), Path.of(inputFile), page)) {

			Map<String, Object> record;
			while ((record = reader.readOne()) != null && count < maxRows) {
				String data = mapper.writeValueAsString(record);
				if (debug)
					System.out.println(data);
				count++;
			}
		}
		long end = System.currentTimeMillis();
		if (debug)
			System.err.printf("Processed %d records in %d ms%n", count, (end - start));
	}

}
