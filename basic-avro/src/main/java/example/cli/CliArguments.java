package example.cli;

import java.io.File;

public class CliArguments {

	private final String inputJsonPath;
	private final String outputAvscPath;

	public CliArguments(String inputJsonPath, String outputAvscPath) {
		if (inputJsonPath == null || inputJsonPath.trim().isEmpty()) {
			throw new IllegalArgumentException("Input JSON path cannot be null or empty");
		}
		if (outputAvscPath == null || outputAvscPath.trim().isEmpty()) {
			throw new IllegalArgumentException("Output AVSC path cannot be null or empty");
		}

		this.inputJsonPath = inputJsonPath.trim();
		this.outputAvscPath = outputAvscPath.trim();
	}

	public static CliArguments parse(String[] args) {
		if (args == null || args.length < 2) {
			throw new IllegalArgumentException(
					"Usage: java -cp target/demo-1.0-SNAPSHOT.jar example.App <input.json> <output.avsc>");
		}

		return new CliArguments(args[0], args[1]);
	}

	public void validateInputExists() {
		File inputFile = new File(inputJsonPath);
		if (!inputFile.exists()) {
			throw new IllegalArgumentException("Input file does not exist: " + inputJsonPath);
		}
		if (!inputFile.isFile()) {
			throw new IllegalArgumentException("Input path is not a file: " + inputJsonPath);
		}
		if (!inputFile.canRead()) {
			throw new IllegalArgumentException("Input file is not readable: " + inputJsonPath);
		}
	}

	public void validateOutputWritable() {
		File outputFile = new File(outputAvscPath);
		File parentDir = outputFile.getParentFile();

		if (parentDir != null && !parentDir.exists()) {
			return;
		}

		if (parentDir != null && !parentDir.canWrite()) {
			throw new IllegalArgumentException("Output directory is not writable: " + parentDir.getAbsolutePath());
		}
	}

	public String getInputJsonPath() {
		return inputJsonPath;
	}

	public String getOutputAvscPath() {
		return outputAvscPath;
	}

	@Override
	public String toString() {
		return "CliArguments{" + "inputJsonPath='" + inputJsonPath + '\'' + ", outputAvscPath='" + outputAvscPath + '\''
				+ '}';
	}
}
