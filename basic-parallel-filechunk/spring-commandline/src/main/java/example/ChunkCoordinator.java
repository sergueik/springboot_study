package example;

import java.util.ArrayList;
import java.util.List;
import example.FileChunk;

public class ChunkCoordinator {
	private final long fileSize;
	private final long approxChunkSize;

	ChunkCoordinator(long fileSize, long approxChunkSize) {
		this.fileSize = fileSize;
		this.approxChunkSize = approxChunkSize;
	}

	List<FileChunk> calculateChunks() {
		List<FileChunk> chunks = new ArrayList<>();

		long offset = 0;
		int id = 0;

		while (offset < fileSize) {
			long end = Math.min(offset + approxChunkSize, fileSize);
			chunks.add(new FileChunk(id++, offset, end));
			offset = end;
		}

		return chunks;
	}
}
