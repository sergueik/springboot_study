package example;

public class FileChunk {
	private final long start;
	private final long size;
	private final int index;

	public FileChunk(long start, long size, int index) {
		this.start = start;
		this.size = size;
		this.index = index;
	}

	public long getStart() {
		return start;
	}

	public long getSize() {
		return size;
	}

	public int getIndex() {
		return index;
	}
}
