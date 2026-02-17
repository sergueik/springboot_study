package example;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class ChunkStream extends InputStream {

	private final FileChannel fileChannel;
	private final long endOffset;
	private long remaining;

	public ChunkStream(final FileChannel fileChannel, final long startOffset, final long endOffset) {

		this.fileChannel = fileChannel;
		this.remaining = endOffset - startOffset;
		this.endOffset = endOffset;
	}

	@Override
	public int read() throws IOException {
		if (0 >= remaining)
			return -1;

		ByteBuffer byteBuffer = ByteBuffer.allocate(1);
		int bytesRead = fileChannel.read(byteBuffer);
		if (bytesRead == -1)
			return -1;
		remaining--;
		return byteBuffer.get(0) & 0xFF;
	}

	@Override
	public int read(byte[] bytes, int offset, int length) throws IOException {
		if (0 >= remaining)
			return -1;
		int bytesToRead = (int) Math.min(length, remaining);
		ByteBuffer byteBuffer = ByteBuffer.wrap(bytes, offset, bytesToRead);
		int bytesRead = fileChannel.read(byteBuffer);

		if (bytesRead == -1)
			return -1;
		remaining -= bytesRead;
		return bytesRead;
	}

	@Override
	public void close() throws IOException {
		// not close
	}

}
