package net.sf.cobolToJson.zTest.json2cbl;

import java.io.IOException;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.cobolToJson.def.ICobol2Json;

public interface ITestData {

	ICobol2Json getCobol2Json();

	AbstractLine cobolDataLine(int lineNumber) throws IOException;

}