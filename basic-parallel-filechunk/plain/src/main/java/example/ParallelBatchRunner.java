package example;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

import example.FileChunk;
import example.ChunkWorker;
import example.utils.PretendJRecord;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ParallelBatchRunner {

	private final Logger logger = LoggerFactory.getLogger(ParallelBatchRunner.class);
	private static boolean debug = false;

	public static void main(String[] args) throws Exception {
		Map<String, String> cli = parseArgs(args);
		String file = "example.bin";
		Long records = 20L;
		int size = 64;
		String page = "cp037";

		if (cli.containsKey("help") || !cli.containsKey("file") || !cli.containsKey("records")) {
			System.err.println(String
					.format("Usage: %s " + "-file <filename> -page <ACP> -records <number> -size <number>\r\n", "jar"));
			return;
		}
		if (cli.containsKey("file"))
			file = cli.get("file");
		if (cli.containsKey("records"))
			records = Long.parseLong(cli.get("records"));
		if (cli.containsKey("size"))
			size = Integer.parseInt(cli.get("size"));
		generateTestFile(Paths.get(file), records, size); // 20 records, 64 bytes each
		new ParallelBatchRunner().run(Paths.get(file));
	}

	public void run(Path file) throws Exception {

		logger.info("Runner hash={}", System.identityHashCode(this));

		long chunkSize = pretendJRecordDetermineChunkSize();
		System.out.println("Pretend JRecord chunk size = " + chunkSize);

		List<FileChunk> chunks = chunkFile(file, chunkSize);

		ExecutorService executor = Executors.newFixedThreadPool(4);
		List<Future<?>> futures = new ArrayList<>();

		for (FileChunk chunk : chunks) {
			ChunkWorker worker = new ChunkWorker(file, chunk);
			logger.info("Worker hash={}", System.identityHashCode(worker));
			futures.add(executor.submit(worker));
		}

		for (Future<?> f : futures) {
			f.get();
		}

		executor.shutdown();
		logger.info("ALL JOBS FINISHED");
	}

	// ---------------- PRETEND JRECORD ----------------

	private long pretendJRecordDetermineChunkSize() {
		PretendJRecord parser = new PretendJRecord();
		System.out.println("PretendJRecord hash=" + System.identityHashCode(parser));
		return 128;
	}

	// ---------------- CHUNKING ----------------

	private List<FileChunk> chunkFile(Path file, long chunkSize) throws IOException {
		long fileSize = Files.size(file);

		List<FileChunk> chunks = new ArrayList<>();
		int index = 0;

		for (long pos = 0; pos < fileSize; pos += chunkSize) {
			long size = Math.min(chunkSize, fileSize - pos);
			chunks.add(new FileChunk(pos, size, index++));
		}

		return chunks;
	}

	static void generateTestFile(Path file, long records, int recordSize) throws IOException {

		try (OutputStream out = Files.newOutputStream(file)) {
			for (long i = 1; i <= records; i++) {
				byte b = (byte) ('0' + ((int) i % 10));
				for (int j = 0; j < recordSize; j++) {
					out.write(b);
				}
			}
		}

		System.out.println("Generated file: " + file.toAbsolutePath());
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
