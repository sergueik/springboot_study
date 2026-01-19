package net.sf.cobolToJson.def;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Details.AbstractLine;

public interface ICobolRecordToJsonObjectIn {

	/**
	 * Set Cobol record (as as a JRecord Line)
	 * @param data Cobol-Record (JRecord Line)
	 * @return
	 */
	ICobolRecordToJsonObjectOut setCobolRecord(AbstractLine line);

	/**
	 * Set Cobol record (as an Array of bytes)
	 * @param data Cobol-Record (data)
	 * @return
	 */
	ICobolRecordToJsonObjectOut setCobolRecord(byte[] data);

	/**
	 * Read a Cobol data record from an input file
	 * @param fileName file name of the cobol data file
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolRecordToJsonObjectOut setInputCobolDataFile(String fileName) throws IOException;

	/**
	 * Read a Cobol data record from an input stream.
	 * @param dataStream dataStream (holding Cobol Data)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolRecordToJsonObjectOut setInputCobolDataFile(InputStream dataStream) throws IOException;

}