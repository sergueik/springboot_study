package net.sf.cobolToJson.def;

import java.io.IOException;
import java.io.InputStream;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineReader;

public interface ICobolMultipleRecordsToArrayIn {

	/**
	 * Read a Cobol data input file
	 * @param fileName file name of the cobol data file
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolMultipleRecordsToArrayOut setInputCobolDataFile(String fileName) throws IOException;

	/**
	 * Read a Cobol data file from an input stream.
	 * @param dataStream dataStream (holding Cobol Data)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolMultipleRecordsToArrayOut setInputCobolDataFile(InputStream dataStream) throws IOException;

	/**
	 * Set the Cobol reader
	 * @param dataReader JRecord (reader for COBOL data files)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolMultipleRecordsToArrayOut setInputCobolData(AbstractLine... lines) throws IOException;

	/**
	 * Set the Cobol reader
	 * @param dataReader JRecord (reader for COBOL data files)
	 * @return This object so you can convert the Cobol Data to a JSON.
	 * @throws IOException
	 */
	ICobolMultipleRecordsToArrayOut setInputCobolDataFile(AbstractLineReader dataReader) throws IOException;

}