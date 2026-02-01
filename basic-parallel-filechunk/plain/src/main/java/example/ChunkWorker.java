package example;

import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.utils.PretendJRecord;

public class ChunkWorker implements Runnable {

	private final Logger logger = LoggerFactory.getLogger(ChunkWorker.class);

	private final Path file;
	private final FileChunk chunk;

	public ChunkWorker(Path file, FileChunk chunk) {
		this.file = file;
		this.chunk = chunk;
	}

	@Override
	public void run() {
		String thread = Thread.currentThread().getName();

		PretendJRecord parser = new PretendJRecord();

		logger.info("Thread={} WorkerHash={} ParserHash={} Chunk={}{}", thread, System.identityHashCode(this),
				System.identityHashCode(parser), chunk.getIndex());

		try (FileChannel fc = FileChannel.open(file, StandardOpenOption.READ)) {

			logger.info("Thread={} FileChannelHash={}{}", thread, System.identityHashCode(fc));

			fc.position(chunk.getStart());

			ByteBuffer buffer = ByteBuffer.allocate((int) chunk.getSize());
			logger.info("Thread={} BufferHash={}{}", thread, System.identityHashCode(buffer));

			fc.read(buffer);
			buffer.flip();

			parser.parse(buffer, chunk, thread);

		} catch (Exception e) {
			e.printStackTrace();
		}
	}
}
