package example;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFileStructureConstants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

@Component
public class Runner implements CommandLineRunner {

	private static final Logger log = LoggerFactory.getLogger(Runner.class);

	@Value("${app.copybookPath}")
	private String copybookPath;

	@Value("${app.dataPath}")
	private String dataPath;

	@Value("${app.font}")
	private String font;

	@Value("${app.chunkSize}")
	private long chunkSize;

	private final ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	@Override
	public void run(String... args) throws Exception {

		Path dataFile = Path.of(dataPath);
		Path copybookFile = Path.of(copybookPath);

		long fileSize = FileChannel.open(dataFile, StandardOpenOption.READ).size();

		ChunkPartitioner chunkPartitioner = new ChunkPartitioner(copybookFile.toAbsolutePath().toString(), fileSize,
				chunkSize);
		List<FileChunk> chunks = chunkPartitioner.partitionFile();

		log.info("Calculated {} chunks", chunks.size());

		List<CompletableFuture<Void>> futures = new ArrayList<>();

		for (FileChunk chunk : chunks) {
			CompletableFuture<Void> future = CompletableFuture.runAsync(() -> processChunk(copybookFile, dataFile, chunk), executor)
					.exceptionally(e -> {
						log.error("Chunk {} failed with {}", chunk.getId(), e.getMessage(), e);
						return null;
					});

			futures.add(future);
		}

		CompletableFuture.allOf(futures.toArray(new CompletableFuture[0])).join();
		executor.shutdown();

		log.info("All chunks processed");
	}

	private void processChunk(Path copybookFile, Path inputFile, FileChunk chunk) {
		LineProcessor lineProcessor = new LineProcessor();
		ChunkWorker chunkWorker = new ChunkWorker(copybookFile, inputFile, chunk, lineProcessor, font);
		log.info("Processing chunk {} [{} - {}]", chunk.getId(), chunk.getStartOffset(), chunk.getEndOffset());
		chunkWorker.run();
	}
}
