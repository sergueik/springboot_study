package example.utils;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

import example.FileChunk;
import example.ParallelBatchRunner;
import example.ChunkWorker;
import example.utils.PretendJRecord;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class Generator {
	private final static Logger logger = LoggerFactory.getLogger(ParallelBatchRunner.class);
	private static boolean debug = false;
	private String filePath;
	private String copybookPath;
	private Long records;
	private int size;
	private String page = "cp037";

	public Generator(String filePath, String copybookPath, String page, Long records, int size) {
		this.filePath = filePath;
		this.copybookPath = copybookPath;
		this.records = records;
		this.size = size;
		this.page = page;
	}

	public void generateTestFile() throws IOException {
		Path file = Paths.get(filePath);
		Path copybook = Paths.get(copybookPath);

		logger.info("Generating file: {} with copybook {} records {}", file.toAbsolutePath(), copybook.toAbsolutePath(),
				records);
		try (OutputStream out = Files.newOutputStream(file)) {
			for (long i = 1; i <= records; i++) {
				byte b = (byte) ('0' + ((int) i % 10));
				for (int j = 0; j < size; j++) {
					out.write(b);
				}
			}
		}

		logger.info("Generated file: {}", file.toAbsolutePath());
	}

}
