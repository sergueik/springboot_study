package info.fetter.rrdclient.util;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.nio.ByteBuffer;

public class FetchServer extends PseudoServer {
	File responseFile;

	public FetchServer(int port, File responseFile) throws IOException {
		super(port);
		this.responseFile = responseFile; 
	}

	@Override
	protected ByteBuffer respond(String request) {
		ByteBuffer response = ByteBuffer.allocate(10*1024*1024);
		try {
			BufferedReader reader = new BufferedReader(new FileReader(responseFile));
			String line;
			while((line = reader.readLine()) != null) {
				logger.trace("Sending line : "+ line);
				response.put(line.getBytes());
				response.putChar('\n');
			}
			reader.close();
			response.flip();
		} catch(IOException e) {
			throw new RuntimeException(e);
		}
		return response;
	}

}
