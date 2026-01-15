package net.sf.cobolToJson.def;

import java.util.List;

//import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.RecordDecider;
import net.sf.JRecord.ExternalRecordSelection.ExternalSelection;
import net.sf.JRecord.Option.IRecordPositionOption;
import net.sf.JRecord.def.IO.builders.ISchemaIOBuilder;
import net.sf.JRecord.def.IO.builders.Icb2xmlLoadOptions;
import net.sf.JRecord.schema.IArrayItemCheck;
import net.sf.JRecord.schema.jaxb.interfaces.IFormatField;
import net.sf.JRecord.schema.jaxb.interfaces.IRedefineSelection;
import net.sf.JRecord.schema.jaxb.interfaces.IWriteCheck;


/**
 * Class To convert <i>Cobol Data Files</i> to/from <i>Json Data files</i> using
 * a Cobol Copybook, This class defines a "Builder" interface for loading the
 * Cobol Copybook
 * 
 * @author Bruce Martin
 *
 */
public interface ICobolJsonConversion extends Icb2xmlLoadOptions {
	/*
	 * Note: This class extends Icb2xmlLoadOptions to pickup method descriptions
	 * in the javadoc
	 */
	
	
	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setFileOrganization(int fileOrganization);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setSplitCopybook(int splitCopybook);

	/**
	 * Set the Cobol Dialect; Possible values include
	 * <ul>
	 * <li><b>ICopybookDialects.FMT_MAINFRAME</b> - Mainframe Cobol
	 * <li><b>ICopybookDialects.FMT_FUJITSU</b> - Written for the old Fujitsu Cobol
	 * 3 compiler
	 * <li><b>ICopybookDialects.FMT_GNU_COBOL</b> - GNU Cobol (formerly Open Cobol)
	 * on a Little Endian machine (e.g Intel).
	 * <li><b>ICopybookDialects.FMT_OC_MICRO_FOCUS_BE</b> - GNU Cobol running in
	 * Microfocus compatibility mode on a Big Endian machine </ul
	 * 
	 * @param dialect new Cobol Dialect
	 */
	ICobolJsonConversion setDialect(int dialect);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setFont(String font);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setInitToSpaces(boolean initToSpaces);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setRecordSelection(String recordName, ExternalSelection selectionCriteria);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setRecordDecider(RecordDecider recordDecider);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setRecordPositionCode(String recordName, IRecordPositionOption positionOption);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setCopybookFileFormat(int copybookFileFormat);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setDropCopybookNameFromFields(boolean dropCopybookNameFromFields);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setPrettyPrint(boolean prettyPrint);

	/**
	 * Whether to add a field name to the main array
	 * 
	 * @param nameMainArray wether to name the main JSON array
	 * @return Builder for more updates
	 */
	ICobolJsonConversion setNameMainArray(boolean nameMainArray);

	/**
	 * Wether to flatten the JSon output or not
	 * 
	 * @param flatten wether to flatyten the JSon output
	 * @return Builder for more updates
	 */
	ICobolJsonConversion setFlattenStructure(boolean flatten);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setArrayCheck(String arrayName, IArrayItemCheck check);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setFormatField(String fieldName, IFormatField formatField);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setWriteCheck(String groupName, IWriteCheck writeCheck);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setRedefineSelection(String groupName, IRedefineSelection redefineSelection);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setArrayCheck(List<String> groupNames, IArrayItemCheck check);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setFormatField(List<String> groupNames, IFormatField formatField);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setWriteCheck(List<String> groupNames, IWriteCheck writeCheck);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setRedefineSelection(List<String> groupNames, IRedefineSelection redefineSelection);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setTagFormat(int tagFormat);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setRecordParent(String recordName, String parentName);

	/**
	 * {@inheritDoc}
	 */
	ICobolJsonConversion setRootRecord(String recordName);

	/**
	 * 
	 * @return an IOBuilder related to the Cobol JSON Conversion
	 */
	ISchemaIOBuilder asIOBuilder();

	/**
	 * Convert multiple COBOL Records to a JSON array
	 * @return <i>Multiple Record Conversion Dialog</i> class
	 */
	ICobolMultipleRecordsToArrayIn multipleRecordsToJsonArray();
	
	/**
	 * Convert a Single COBOL record to a JSON object
	 * @return <i>Single Record Conversion Dialog</i>
	 */
	ICobolRecordToJsonObjectIn singleRecordToJsonObject();
	
	/**
	 * Convert a JSON array to multiple COBOL Records
	 * @return <i>JSON Array to Multiple COBOL records dialog</i> class
	 */
	IJsonArrayToMultipleCobolRecordsIn jsonArrayToMultipleRecords();
	
	/**
	 * Convert a JSON object into a single COBOL record
	 * @return <i>JSON Object to single COBOL record dialog</i> class
	 */
	IJsonObjectToCobolRecordIn jsonObjectToSingleRecord();
}
