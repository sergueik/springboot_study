package example;

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

	// pretend this comes from application.yml
	private final long approxChunkSizeBytes = 5 * 1024 * 1024; // 5MB

	private AbstractLineReader reader;
	private LayoutDetail layout;
	private String copybookPath;
	private String font;

	private final ExecutorService executor = Executors.newFixedThreadPool(Runtime.getRuntime().availableProcessors());

	@Override
	public void run(String... args) throws Exception {

		Path inputFile = Path.of("input.dat");

		long fileSize = FileChannel.open(inputFile, StandardOpenOption.READ).size();

		ChunkCoordinator coordinator = new ChunkCoordinator(fileSize, approxChunkSizeBytes);
		List<FileChunk> chunks = coordinator.calculateChunks();

		log.info("Calculated {} chunks", chunks.size());

		List<CompletableFuture<Void>> futures = new ArrayList<>();

		for (FileChunk chunk : chunks) {
			CompletableFuture<Void> future = CompletableFuture.runAsync(() -> processChunk(inputFile, chunk), executor)
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

	private void processChunk(Path inputFile, FileChunk chunk) {
		LineProcessor lineProcessor = new LineProcessor();
		ChunkWorker chunkWorker = new ChunkWorker(inputFile, chunk,  lineProcessor,  font);
		log.info("Processing chunk {} [{} - {}]", chunk.getId(), chunk.getStartOffset(), chunk.getEndOffset());
		chunkWorker.run();
		/*
		try (FileChannel channel = FileChannel.open(inputFile, StandardOpenOption.READ)) {

			long size = chunk.getEndOffset() - chunk.getStartOffset();

			ByteBuffer buffer = channel.map(FileChannel.MapMode.READ_ONLY, chunk.getStartOffset(), size);

			ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL.newIOBuilder(inputFile.toString())
					.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH).setFont(font); // EBCDIC

			FileChannel fileChannel = FileChannel.open(inputFile.toAbsolutePath(), StandardOpenOption.READ);
			InputStream inputStream = new BoundedChannelInputStream(fileChannel, chunk.getStartOffset(),
					chunk.getEndOffset());

			// Each worker constructs its own JRecord reader (thread safe by isolation)
			AbstractLineReader reader = ioBuilder.newReader(inputStream);
			AbstractLine line = reader.read();
			while (line != null) {
				handleLine(line, chunk.getId());
				line = reader.read();
				if (line == null)
					return;

			}
			reader.close();

		} catch (Exception e) {
			throw new RuntimeException("Chunk " + chunk.getId() + " failed", e);
		}
		*/
	}
/*
	private void handleLine(AbstractLine line, int chunkId ) {
		// simple logging handler
		Map<String, Object> record = new LinkedHashMap<>();
		LayoutDetail layoutDetail = line.getLayout();
		RecordDetail recordDetail = layoutDetail.getRecord(0);
		for (FieldDetail fieldDetail : recordDetail.getFields()) {

			String fieldName = fieldDetail.getName();
			IFieldValue fieldValue = line.getFieldValue(fieldName);
			// NOTE: need to use asObject()
			Object value = fieldValue.asString().trim();
			record.put(fieldName, value);
			log.info("Chunk {} Record: {}", chunkId, line.getFieldValue(0, 0).asString());
		}
	}
	
	*/
}
