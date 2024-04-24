package example.utils;

import java.io.File;
import java.io.IOException;
import java.io.InputStream;

public class Utils {

	public static String getFileContent(String fileName) {
		try {
			final InputStream stream = Utils.class.getClassLoader().getResourceAsStream(fileName);
			final byte[] bytes = new byte[stream.available()];
			stream.read(bytes);
			return new String(bytes, "UTF-8");
		} catch (IOException e) {
			throw new RuntimeException(fileName);
		}
	}

	public static String listDirecroryFiles() {
		return listDirecroryFiles("c:/temp/upload");
	}

	// origin:
	// http://www.java2s.com/ref/java/java-file-list-all-files-and-directories-under-a-folder.html
	public static String listDirecroryFiles(String uploadDir) {
		String line = null;
		// NOTE: not checking that directory exists. This is just minimal demo
		// example
		File dir = new File(uploadDir);
		if (dir.exists()) {
			File[] listFiles = dir.listFiles();
			if (listFiles.length > 0) {
				return formattedList(listFiles);
			} else {
				return "";
			}
		} else {
			return "";
		}

	}

	// based on:
	// https://stackoverflow.com/questions/13669430/formated-output-in-java-like-ls
	private static String formattedList(File[] files) {
		final int NUM_COLUMNS = 3;
		final int SEPARATING_SPACE_LENGTH = 2;
		StringBuffer processOutput = new StringBuffer();
		String line = null;
		int[] maxLength = new int[NUM_COLUMNS];

		for (int i = 0; i < files.length; i++) {
			File f = files[i];
			if (f.isFile()) {
				line = f.getName();
			} else if (f.isDirectory()) {
				line = f.getName() + "/";
			}
			int fileLength = line.length();
			int columnIndex = i % NUM_COLUMNS;

			if (maxLength[columnIndex] < fileLength) {
				maxLength[columnIndex] = fileLength;
			}
		}

		for (int i = 0; i < files.length; i++) {
			File f = files[i];
			if (f.isFile()) {
				line = f.getName();
			} else if (f.isDirectory()) {
				line = f.getName() + "/";
			}
			processOutput.append(line);

			for (int j = 0; j < maxLength[i % NUM_COLUMNS] - line.length() + SEPARATING_SPACE_LENGTH; j++) {
				processOutput.append(" ");
			}

			if ((i + 1) % NUM_COLUMNS == 0) {
				processOutput.append("\n");
			}
		}
		return processOutput.toString();
	}

}
