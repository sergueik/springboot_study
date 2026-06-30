package example.service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.springframework.stereotype.Service;

import io.github.cdimascio.dotenv.Dotenv;

@Service
public class DotnetConfigService {
	private static final Logger logger = LoggerFactory.getLogger(DotnetConfigService.class);

	public void load() throws Exception {
		var dotenv = Dotenv.configure().ignoreIfMissing().load();
		logger.info("Working dir: {}", System.getProperty("user.dir"));

		dotenv.entries().forEach(e -> logger.info("dotenv: {}={}", e.getKey(), e.getValue()));
		dotenv.entries()
				.forEach(entry -> System.setProperty(entry.getKey().toLowerCase().replace('_', '.'), entry.getValue()));

		logger.info("vite.tus.chunk.size={}", System.getProperty("vite.tus.chunk.size"));
	}
}
