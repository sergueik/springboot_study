package example.utils;

import java.nio.ByteBuffer;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.ChunkWorker;
import example.FileChunk;

public class PretendJRecord {

	private final Logger logger = LoggerFactory.getLogger(PretendJRecord.class);

	public void parse(ByteBuffer buffer, FileChunk chunk, String thread) {
		long count = 0;
		byte first = buffer.get(0);

		while (buffer.hasRemaining()) {
			buffer.get();
			count++;
		}

		logger.info("Thread={} PretendJRecordHash={} Chunk={} FirstByte={} Bytes={}{}", thread,
				System.identityHashCode(this), chunk.getIndex(), (char) first, count);
	}

}
