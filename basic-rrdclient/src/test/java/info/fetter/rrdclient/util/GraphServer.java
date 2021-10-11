package info.fetter.rrdclient.util;

import java.io.File;
import java.io.IOException;
import java.io.RandomAccessFile;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;

public class GraphServer extends PseudoServer {
	File image;

	public GraphServer(int port, File image) throws IOException {
		super(port);
		this.image = image;
	}

	@Override
	protected ByteBuffer respond(String request) {
		try {
			RandomAccessFile file = new RandomAccessFile(image, "r");
			FileChannel channel = file.getChannel();
			long fileSize = channel.size();
			ByteBuffer response = ByteBuffer.allocate((int) fileSize + 100);
			channel.read(response);
			response.put("OK u:0.00 s:0.00 r:0.00\n".getBytes());
			response.flip();
			file.close();
			return response;
		} catch (Exception e) {
			throw new RuntimeException(e);
		}
	}

}
