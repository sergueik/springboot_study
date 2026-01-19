package net.sf.cobolToJson.impl.readJson;

import java.io.IOException;

public interface IProcessFields {
	/**
	 * end of an object
	 * @param level level of the object (0 top level object)
	 * @param fieldName object name
	 */
	void endObject(int level, boolean endOfLine, String fieldName);
	/**
	 * Update a field with an integer value
	 * @param fieldName name of field being updated
	 * @param value value of the field
	 */
	void updateField(String fieldName, long value);
	/**
	 * Update a field with an String value
	 * @param fieldName name of field being updated
	 * @param value value of the field
	 */
	void updateField(String fieldName, String value);
	void close() throws IOException;
	/**
	 * 
	 * @return whether it is a single object (COBOL data line) or an array
	 * of objects  (array of COBOL data line)
	 */
	boolean isSingleObject();
}
