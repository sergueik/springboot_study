package example.utils;

import java.io.IOException;
import java.io.InputStream;

public class Utils {

	public static String getFileContent(String fileName) {
		try {
			final InputStream stream = Utils.class.getClassLoader()
					.getResourceAsStream(fileName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(fileName);
		}
	}

}
