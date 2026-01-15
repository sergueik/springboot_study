package net.sf.JRecord.util.errorLog;

import java.util.ArrayList;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Details.AbstractLine;

public class BasicErrorLog implements ILogLinesInError {

	private final ArrayList<AbstractLine> lines = new ArrayList<>();
	private final ArrayList<String> messages = new ArrayList<>();
	private final ArrayList<Integer> lineNumbers = new ArrayList<>();
	private int linesToStore = 500, errorLineCount = 0;
	
	@Override
	public void logErrorLine(int lineNumber, AbstractLine line) {
		logErrorLine(lineNumber, line, "Invalid Record Type");
	}
	
	@Override
	public void logErrorLine(int lineNumber, AbstractLine line, String message) {
		if (errorLineCount++ < linesToStore) {
			lines.add(line);
			messages.add(message);
			lineNumbers.add(lineNumber);
		}
	}


	@Override
	public void reportErrors() {
		if (errorLineCount > 0) {
			boolean binaryFile = lines.get(0).getLayout().isBinary();
			printErrorLine("Errors found in the Conversion");
			for (int idx = 0; idx < lines.size(); idx++) {
				printErrorLine("");
				printErrorLine("Line Number: " + lineNumbers.get(idx) + " Error: " + messages.get(idx));
				String strLine = truncateTo100chars(lines.get(idx).getFullLine());
				printErrorLine(strLine);
				
				if (binaryFile) {
					byte[] data = lines.get(idx).getData();
					if (data != null && data.length > 0) {
						int max = Math.min(100, data.length);
						printErrorLine(Conversion.getDecimalSB(data, 0, max).toString());
					}
				}
			}
			throw new RuntimeException(errorLineCount + " Invalid Records found in the file. See the error messages. "
					+ "Errors start at " + lineNumbers.get(0) + " >>" + truncateTo100chars(lines.get(0).getFullLine()));
		}
	}

	protected void printErrorLine(String m) {
		System.err.println(m);
	}

	private String truncateTo100chars(String strLine) {
		strLine = strLine == null || strLine.length() < 100 ? strLine : strLine.substring(0, 100);
		return strLine;
	}

	
}
