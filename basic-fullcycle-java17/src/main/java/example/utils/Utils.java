package example.utils;

import java.io.IOException;
import java.io.InputStream;
import java.io.File;

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

	// orign:
	// http://www.java2s.com/ref/java/java-file-list-all-files-and-directories-under-a-folder.html
	public static String listDirecroryFiles() {
		StringBuffer processOutput = new StringBuffer();
		processOutput.append("");
		String line = null;
		// NOTE: no checking that directory exists. This is just minimal demo exampe
		File dir = new File("c:/temp/upload");
		File[] list = dir.listFiles();
		if (list.length > 0) {
			for (File f : list) {
				if (f.isFile()) {
					line = f.getPath() + " (File)";
					processOutput.append(line);
					processOutput.append("\n");
				} else if (f.isDirectory()) {
					line = f.getPath() + " (Directory)";
					processOutput.append(line);
					processOutput.append("\n");
				}
			}
		}
		return processOutput.toString();
	}
}
