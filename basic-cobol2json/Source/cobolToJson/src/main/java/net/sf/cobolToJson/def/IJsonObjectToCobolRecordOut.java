package net.sf.cobolToJson.def;

import java.io.IOException;
import java.io.OutputStream;

import net.sf.JRecord.Details.AbstractLine;

public interface IJsonObjectToCobolRecordOut {

	/**
	 * Convert the JSON to Cobol Data Records and write the Cobol data to a specified file
	 * @param cobolDataFileName name of the output Cobol data file
	 * @throws IOException
	 */
	void writeCobolDataFile(String cobolDataFileName) throws IOException;

	/**
	 * Convert the JSON to Cobol Data Records and write them to a stream
	 * @param cobolDataStream stream where the Cobol records are written.
	 * @throws IOException
	 */
	void writeCobolDataFile(OutputStream cobolDataStream) throws IOException;

	/**
	 * Convert the JSON to Cobol Data Records and return them as a list
	 * @return Cobol data
	 * @throws IOException
	 */
	AbstractLine toCobolDataLine() throws IOException;

}