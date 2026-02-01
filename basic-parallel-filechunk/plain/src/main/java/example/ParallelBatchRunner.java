package example;

import java.io.*;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;

import example.FileChunk;
import example.ChunkWorker;
import example.utils.Generator;
import example.utils.PretendJRecord;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class ParallelBatchRunner {

	private final static Logger logger = LoggerFactory.getLogger(ParallelBatchRunner.class);
	private static boolean debug = false;

	public static void main(String[] args) throws Exception {
		Map<String, String> cli = parseArgs(args);
		String file = "example.bin";
		String copybook = "example.cbl";
		Long records = 20L;
		int size = 64;
		int pool = 4;
		String page = "cp037";

		if (cli.containsKey("help") || !cli.containsKey("file") || !cli.containsKey("records")) {
			System.err.println(String.format(
					"Usage: %s -file <filename> -page <ACP> -records <number> -size <number> -copybook <filename> -pool <number>\r\n",
					"jar"));
			return;
		}
		if (cli.containsKey("file"))
			file = cli.get("file");
		if (cli.containsKey("page"))
			page = cli.get("page");
		if (cli.containsKey("copybook"))
			copybook = cli.get("copybook");
		if (cli.containsKey("records"))
			records = Long.parseLong(cli.get("records"));
		if (cli.containsKey("size"))
			size = Integer.parseInt(cli.get("size"));
		if (cli.containsKey("pool"))
			pool = Integer.parseInt(cli.get("pool"));
		if (cli.containsKey("generate")) {
			new Generator(file, copybook, page, records, size).generateTestFile();
			return;
		}
		new ParallelBatchRunner().run(Paths.get(file), pool);
	}

	public void run(Path file, int poolSize) throws Exception {

		logger.info("Runner hash={}", System.identityHashCode(this));

		long chunkSize = pretendJRecordDetermineChunkSize();
		System.out.println("Pretend JRecord chunk size = " + chunkSize);

		List<FileChunk> chunks = chunkFile(file, chunkSize);

		ExecutorService executor = Executors.newFixedThreadPool(poolSize);
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
