package net.sf.cobolToJson.impl;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.cobolToJson.def.ICobolRecordToJsonObjectIn;
import net.sf.cobolToJson.def.ICobolRecordToJsonObjectOut;

/**
 * Convert a Single Cobol Record to a JSON Object
 * 
 * @author Bruce Martin
 *
 */
public class CobolRecordToJsonObject implements ICobolRecordToJsonObjectOut, ICobolRecordToJsonObjectIn {
	private final Cobol2JsonImp cbl2json;
	private AbstractLine line;
	

	/**
	 * Convert a Single Cobol Record to a JSON Object
	 * @param cbl2json Cobl2Json conversion object
	 */
	public CobolRecordToJsonObject(Cobol2JsonImp cbl2json) {
		super();
		this.cbl2json = cbl2json;
	}

	

	/**
	 * Set Cobol record (as as a JRecord Line)
	 * @param data Cobol-Record (JRecord Line)
	 * @return
	 */
	@Override
	public CobolRecordToJsonObject setCobolRecord(AbstractLine line) {
		this.line = line;
		return this;
	}

	/**
	 * Set Cobol record (as an Array of bytes)
	 * @param data Cobol-Record (data)
	 * @return
	 */
	@Override
	public CobolRecordToJsonObject setCobolRecord(byte[] data) {
		try {
			this.line = cbl2json.asIOBuilder().newLine(data);
		} catch (IOException e) {
			throw new RuntimeException("Error Creating JRecord Line", e);
		}
		return this;
	}
	
	/**
	 * Read a Cobol data record from an input file
	 * @param fileName file name of the cobol data file
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	public CobolRecordToJsonObject setInputCobolDataFile(String fileName) throws IOException {
		return setInputCobolDataFile(new FileInputStream(fileName));
	}
	
	/**
	 * Read a Cobol data record from an input stream.
	 * @param dataStream dataStream (holding Cobol Data)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	public CobolRecordToJsonObject setInputCobolDataFile(InputStream dataStream) throws IOException {
		ISchemaIOBuilder ioBuilder = cbl2json.asIOBuilder();
		AbstractLineReader reader = ioBuilder.newReader(dataStream);
		
		this.line = reader.read();
		
		reader.close();
		
		return this;
	}
	
	/**
	 * 
	 * @return The Cobol record converted to a JSON string
	 */
	@Override
	public String toJsonString()  {
		StringWriter w = new StringWriter();
		try {
			writeJson(w);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		return w.toString();
	}

	/**
	 * 
	 * @param fileName file name to write the JSON object to
	 * @return this to allow a second destination
	 * @throws IOException 
	 */
	@Override
	public ICobolRecordToJsonObjectOut writeJson(String fileName) throws IOException {
		return writeJson(new FileWriter(fileName));
	}

	/**
	 * Write a single Record as JSON on the supplied writer
	 * @param writer where to write the json
	 * @return this to allow a second destination
	 * @throws IOException
	 */
	@Override
	public ICobolRecordToJsonObjectOut writeJson(Writer writer) throws IOException {
		cbl2json.singleCobolRecord2json(line, writer);
		return this;
	}
}
