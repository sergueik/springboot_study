package net.sf.cobolToJson.impl;

import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.FileReader;
import java.io.IOException;
import java.io.OutputStream;
import java.io.Reader;
import java.io.StringReader;
import java.util.List;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.cobolToJson.def.IJsonArrayToMultipleCobolRecordsIn;
import net.sf.cobolToJson.def.IJsonArrayToMultipleCobolRecordsOut;

public class JsonArrayToMultipleCobolRecords implements IJsonArrayToMultipleCobolRecordsOut, IJsonArrayToMultipleCobolRecordsIn {
	private final Cobol2JsonImp cbl2json;
	
	private Reader readerJsonFile;
	
	
	public JsonArrayToMultipleCobolRecords(Cobol2JsonImp cbl2json) {
		super();
		this.cbl2json = cbl2json;
	}
	
	/**
	 * Set the Input JSON String
	 * @param jsonString input JSON
	 * @return class so you can create the Cobol Output
	 */
	@Override
	public JsonArrayToMultipleCobolRecords setJsonString(String jsonString) {
		return setJsonInput(new StringReader(jsonString));
	}
	
	/**
	 * Set the file name of the input JSON
	 * @param filename file name of the input JSON
	 * @return class so you can create the Cobol Output
	 * @throws FileNotFoundException
	 */
	@Override
	public JsonArrayToMultipleCobolRecords setJsonInputFileName(String filename) throws FileNotFoundException {
		return setJsonInput(new FileReader(filename));
	}
	
	/**
	 * Define the JSON Reader 
	 * @param reader Reader for the JSon data
	 * @return class so you can create the Cobol Output
	 */
	@Override
	public JsonArrayToMultipleCobolRecords setJsonInput(Reader reader) {
		readerJsonFile = reader;
		return this;
	}

	/**
	 * Convert the JSON to Cobol Data Records and write the Cobol data to a specified file
	 * @param cobolDataFileName name of the output Cobol data file
	 * @throws IOException
	 */
	@Override
	public void writeCobolDataFile(String cobolDataFileName) throws IOException {
		writeCobolDataFile(new FileOutputStream(cobolDataFileName));
	}

	/**
	 * Convert the JSON to Cobol Data Records and write them to a stream
	 * @param cobolDataStream stream where the Cobol records are written.
	 * @throws IOException
	 */
	@Override
	public void writeCobolDataFile(OutputStream cobolDataStream) throws IOException {
		checkReaderExists();
		cbl2json.jsonArrayToCobolFile(readerJsonFile, cobolDataStream);
	}


	private void checkReaderExists() {
		if (readerJsonFile == null) {
			throw new RuntimeException("You must supply input JSON File/Reader/String");
		}
	}
	
	/**
	 * Convert the JSON to Cobol Data Records and return them as a list
	 * @return Cobol data
	 * @throws IOException
	 */
	@Override
	public List<AbstractLine> toCobolDataLines() throws IOException {
		checkReaderExists();
		return cbl2json.jsonArrayToCobolLines(readerJsonFile);
	}
}
