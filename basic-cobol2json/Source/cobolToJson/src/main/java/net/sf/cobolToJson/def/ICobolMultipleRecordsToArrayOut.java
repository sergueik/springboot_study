package net.sf.cobolToJson.def;

import java.io.IOException;
import java.io.Writer;

public interface ICobolMultipleRecordsToArrayOut {

	/**
	 *  Write the JSON
	 * @param fileName name of the file where the JSON should be written
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolMultipleRecordsToArrayIn writeJsonArray(String fileName) throws IOException;

	/**
	 * Write the JSON
	 * @param writer where to write the JSON
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolMultipleRecordsToArrayIn writeJsonArray(Writer writer) throws IOException;

	/**
	 * Convert the Cobol Data to a JSON String
	 * @return Cobol Data converted to JSON
	 */
	String toJsonString();

}