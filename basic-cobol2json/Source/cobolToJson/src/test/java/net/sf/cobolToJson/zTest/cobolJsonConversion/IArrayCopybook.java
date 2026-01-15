package net.sf.cobolToJson.zTest.cobolJsonConversion;

import net.sf.JRecord.Details.AbstractLine;

public interface IArrayCopybook {

	String getCopybookString();

	void updateLine(AbstractLine line, int lineNumber);

	String getExpectedSingleRecordJson();

	String getJsonArray();

	String getExpectedJsonArray();

}