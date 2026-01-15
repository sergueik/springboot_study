package net.sf.cobolToJson.def;

import java.io.FileNotFoundException;
import java.io.Reader;

public interface IJsonObjectToCobolRecordIn {

	/**
	 * Set the Input JSON String
	 * @param jsonString input JSON
	 * @return class so you can create the Cobol Output
	 */
	IJsonObjectToCobolRecordOut setJsonString(String jsonString);

	/**
	 * Set the file name of the input JSON
	 * @param filename file name of the input JSON
	 * @return class so you can create the Cobol Output
	 * @throws FileNotFoundException
	 */
	IJsonObjectToCobolRecordOut setJsonInput(String filename) throws FileNotFoundException;

	/**
	 * Define the JSON Reader 
	 * @param reader Reader for the JSon data
	 * @return class so you can create the Cobol Output
	 */
	IJsonObjectToCobolRecordOut setJsonInput(Reader reader);

}