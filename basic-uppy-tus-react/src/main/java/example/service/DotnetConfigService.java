package example.service;

import java.util.HashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.stereotype.Service;

import io.github.cdimascio.dotenv.Dotenv;
import example.utils.MinimalCalculator;

@Service
public class DotnetConfigService {
	private static final Logger logger = LoggerFactory.getLogger(DotnetConfigService.class);

	public void load() throws Exception {
		var dotenv = Dotenv.configure().ignoreIfMissing().load();
		logger.info("Working dir: {}", System.getProperty("user.dir"));

		dotenv.entries().stream().peek(e -> logger.info("dotenv: {}={}", e.getKey(), e.getValue()))
				.map(e -> new HashMap.SimpleEntry<>(e.getKey().toLowerCase().replace('_', '.'), e.getValue()))
				.peek(e -> System.setProperty(e.getKey(), e.getValue()))
				.forEach(e -> logger.debug("sysprop: {}={}", e.getKey(), e.getValue()));

		logger.info("vite.tus.chunk.size={}", System.getProperty("vite.tus.chunk.size"));
	}

	public Map<String, Object> buildConfig() throws Exception {
		this.load();
		Map<String, Object> data = new HashMap<>();
		// chop the "VITE_" prefix.
		// NOTE: if not set chunkSize, tus behaves like: send the whole file in one
		// request (effectively no chunking override)
		if (System.getProperties().containsKey("vite.tus.chunk.size")) {
			// data.put("TUS_CHUNK_SIZE",
			// Long.parseLong(System.getProperty("vite.tus.chunk.size")));
			String chunkSize = System.getProperty("vite.tus.chunk.size");
			// Map<String, Long> data = new HashMap<>();
			if (chunkSize != null && !chunkSize.isBlank()) {
				data.put("TUS_CHUNK_SIZE", MinimalCalculator.evaluate(chunkSize));
			}
		}

		data.put("TUS_ENDPOINT", System.getProperty("vite.tus.endpoint"));
		if (System.getProperties().containsKey("vite.max.number.of.files"))
			data.put("MAX_NUMBER_OF_FILES", Integer.parseInt(System.getProperty("vite.max.number.of.files")));
		if (System.getProperties().containsKey("vite.max.file.size.bytes"))
			data.put("MAX_FILE_SIZE_BYTES", Long.parseLong(System.getProperty("vite.max.file.size.bytes")));
		data.put("TUS_RETRY_DELAYS", System.getProperty("vite.tus.retry.delays").split(","));

		return data;
	}
}
