package net.sf.cobolToJson.util.errorLog;

import net.sf.JRecord.Details.AbstractLine;

public interface ILogLinesInError {

	void logErrorLine(int lineNumber, AbstractLine line);
	void logErrorLine(int lineNumber, AbstractLine line, String message);
	void reportErrors();
}
