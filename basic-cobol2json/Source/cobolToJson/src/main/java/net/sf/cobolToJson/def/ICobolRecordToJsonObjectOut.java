package net.sf.cobolToJson.def;

import java.io.IOException;
import java.io.Writer;

public interface ICobolRecordToJsonObjectOut {

	/**
	 * 
	 * @return The Cobol record converted to a JSON string
	 */
	String toJsonString();

	/**
	 * 
	 * @param fileName file name to write the JSON object to
	 * @return this to allow a second destination
	 * @throws IOException 
	 */
	ICobolRecordToJsonObjectOut writeJson(String fileName) throws IOException;

	/**
	 * Write a single Record as JSON on the supplied writer
	 * @param writer where to write the json
	 * @return this to allow a second destination
	 * @throws IOException
	 */
	ICobolRecordToJsonObjectOut writeJson(Writer writer) throws IOException;

}