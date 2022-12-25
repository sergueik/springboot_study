package example.utils;

import java.io.IOException;
import java.io.InputStream;

public class FileLoaderUtils {

	public static byte[] loadClasspathFile(String fileName) throws IOException {
		return loadClasspathFile(fileName, null);
	}

	public static byte[] loadClasspathFile(String fileName,
			ClassLoader classLoader) throws IOException {
		InputStream inputStream = getInputStream(fileName, classLoader);
		int available = inputStream.available();
		byte[] doc = new byte[available];
		inputStream.read(doc);
		inputStream.close();
		return doc;
	}

	public static InputStream getInputStream(String fileName) throws IOException {
		return getInputStream(fileName, null);
	}

	public static InputStream getInputStream(String fileName,
			ClassLoader classLoader) throws IOException {
		if (classLoader == null) {
			classLoader = Thread.currentThread().getContextClassLoader();
			if (classLoader == null) {
				classLoader = FileLoaderUtils.class.getClassLoader();
			}
		}
		return classLoader.getResourceAsStream(fileName);
	}
}
