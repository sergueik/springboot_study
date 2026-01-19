package net.sf.cobolToJson.impl;

import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringWriter;
import java.io.Writer;
import java.util.Arrays;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.ListLineReader;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.cobolToJson.def.ICobolMultipleRecordsToArrayIn;
import net.sf.cobolToJson.def.ICobolMultipleRecordsToArrayOut;

public class CobolMultipleRecordsToJsonArray implements ICobolMultipleRecordsToArrayIn, ICobolMultipleRecordsToArrayOut {
	private final Cobol2JsonImp cbl2json;
	private AbstractLineReader reader;

	public CobolMultipleRecordsToJsonArray(Cobol2JsonImp cbl2json) {
		super();
		this.cbl2json = cbl2json;
	}

	
	/**
	 * Read a Cobol data input file
	 * @param fileName file name of the cobol data file
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	public CobolMultipleRecordsToJsonArray setInputCobolDataFile(String fileName) throws IOException {
		return setInputCobolDataFile(new FileInputStream(fileName));
	}
	
	/**
	 * Read a Cobol data file from an input stream.
	 * @param dataStream dataStream (holding Cobol Data)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	public CobolMultipleRecordsToJsonArray setInputCobolDataFile(InputStream dataStream) throws IOException {
		ISchemaIOBuilder ioBuilder = cbl2json.asIOBuilder();
		reader = ioBuilder.newReader(dataStream);
 
		return this;
	}
	
	/**
	 * Set the Cobol reader
	 * @param dataReader JRecord (reader for COBOL data files)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	@SuppressWarnings("deprecation")
	public CobolMultipleRecordsToJsonArray setInputCobolData(AbstractLine... lines) throws IOException {
		LayoutDetail layout = cbl2json.asIOBuilder().getLayout();
		for (AbstractLine l : lines) {
			l.setLayout(layout);
		}
		this.reader = new ListLineReader(Arrays.asList(lines), layout);
 
		return this;
	}

	/**
	 * Set the Cobol reader
	 * @param dataReader JRecord (reader for COBOL data files)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	public CobolMultipleRecordsToJsonArray setInputCobolDataFile(AbstractLineReader dataReader) throws IOException {
		this.reader = dataReader;
		 
		return this;
	}
	
	/**
	 * Convert the Cobol Data to a Json String
	 * @return Cobol Data converted to Json
	 */
	@Override
	public String toJsonString()  {
		StringWriter w = new StringWriter();
		try {
			writeJsonArray(w);
		} catch (IOException e) {
			throw new RuntimeException(e);
		}
		
		return w.toString();
	}
	/**
	 *  Write the JSON
	 * @param fileName name of the file where the JSON should be written
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	public ICobolMultipleRecordsToArrayIn writeJsonArray(String fileName) throws IOException {
		return writeJsonArray(new FileWriter(fileName));
	}
	
	/**
	 * Write the JSON
	 * @param writer where to write the JSON
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	@Override
	public ICobolMultipleRecordsToArrayIn writeJsonArray(Writer writer) throws IOException {
		cbl2json.cobol2json(reader, writer);
		
		return this;
	}
}
