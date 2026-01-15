package net.sf.cobolToJson;

import net.sf.JRecord.Option.JRecordConstantVars;
import net.sf.JRecord.schema.ArrayElementChecks;
import net.sf.JRecord.schema.jaxb.impl.StandardFieldFormats;
import net.sf.JRecord.schema.jaxb.impl.StandardItemWriteChecks;
import net.sf.cb2xml.copybookReader.ICobolCopybookTextSource;
import net.sf.cb2xml.def.Cb2xmlConstants;
import net.sf.cobolToJson.def.ICobolJsonConversion;
import net.sf.cobolToJson.impl.Cobol2JsonImp;

/**
 * This class provides a fluent interface for converting Cobol data files to JSON.
 * First you define the Cobol copybook and the attributes associated with it:
 * 
 * <pre>
 *     CobolJsonConversion.newCobolJsonConversion("me/cobol/DTAR020.cbl")
 *         .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
 *         .setSplitCopybook(CopybookLoader.SPLIT_NONE)
 *         .setFont("cp037")
 * </pre>
 * 
 * Then you specify the Conversion e.g
 * 
 * <pre>
 *        .singleRecordToJsonObject()
 *            .setCobolRecord(line.getData())
 *            .toJsonString()
 * </pre>
 * 
 * 
 * @author Bruce Martin
 *
 */
public class CobolJsonConversion {

	public static final JRecordConstantVars JR_CONSTANTS = new JRecordConstantVars();
	public static final ArrayElementChecks  ARRAY_CHECK_BUILDER = ArrayElementChecks.INSTANCE;
	public static final StandardItemWriteChecks COBOL_ITEM_WRITE_CHECKS = StandardItemWriteChecks.INSTANCE;
	public static final StandardFieldFormats FIELD_FORMATS = StandardFieldFormats.INSTANCE;

	/**
	 * Create a Cobol-Data-to-JSON converter from a Cobol Copybook File.
	 * With this method, you define the Cobol Attributes (Dialect, font (encoding), 
	 * file-structure and split options. You can then select one of the Cobol-Json conversion dialogs<ul>
	 * <li><b>multipleRecordsToJsonArray</b> - Convert Multiple COBOL data records to a JSON Array. The input can be an array/file or stream.
	 * <li><b>singleRecordToJsonObject</b> - Convert a single COBOL Record to JSON
	 * <li><b>jsonArrayToMultipleRecords</b> - Convert a JSON Array to Multiple COBOL Records
	 * <li><b>jsonObjectToSingleRecord</b> - Convert a JSON Object to a COBOL Record
	 * </ul>
	 * 
	 * These conversion methods work in a fluent style. For example with the <b>multipleRecordsToJsonArray()</b><ul>
	 * <li>you set the source of the cobol records using the setInputCobolDataFile/setInputCobolData methods 
	 * see  {@link net.sf.cobolToJson.def.ICobolMultipleRecordsToArrayIn}
	 * <li>Then you set the output using the writeJsonArray/toJsonString methods,
	 * see {@link net.sf.cobolToJson.def.ICobolMultipleRecordsToArrayOut}
	 * 
	 * <pre>
	 * Usage:
	 * 
     *     CobolJsonConversion.newCobolJsonConversion("me/cobol/DTAR020.cbl")
     *         .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
     *         .setSplitCopybook(CopybookLoader.SPLIT_NONE)
     *         .setFont("cp037")
     *         .setTagFormat(IReformatFieldNames.RO_UNDERSCORE)
     *
     *        .singleRecordToJsonObject()
     *            .setCobolRecord(line.getData())
     *            .toJsonString()
     * </pre           
     * 
     * @param cobolCopybookFileName Cobol copybook file name
     * @return Cobol2Json builder
     */
    public static ICobolJsonConversion newCobolJsonConversion(String cobolCopybookFileName) {
        return Cobol2JsonImp.newCobolJsonConversion(cobolCopybookFileName);
    }

    /**
	 * Create a Cobol-Data-to-Json converter from a Cobol Copybook File.
	 * With this method, you define the Cobol Attributes (Dialect, font (encoding), 
	 * file-structure and split options. You can then select one of the Cobol-Json conversion dialogs<ul>
	 * <li><b>multipleRecordsToJsonArray</b> - Convert Multiple COBOL data records to a JSON Array. The input can be an array/file or stream.
	 * <li><b>singleRecordToJsonObject</b> - Convert a single COBOL Record to JSON
	 * <li><b>jsonArrayToMultipleRecords</b> - Convert a JSON Array to Multiple COBOL Records
	 * <li><b>jsonObjectToSingleRecord</b> - Convert a JSON Object to a COBOL Record
	 * </ul>
	 * 
	 * These conversion methods work in a fluent style. For example with the <b>multipleRecordsToJsonArray()</b><ul>
	 * <li>you set the source of the cobol records using the setInputCobolDataFile/setInputCobolData methods 
	 * see  {@link net.sf.cobolToJson.def.ICobolMultipleRecordsToArrayIn}
	 * <li>Then you set the output using the writeJsonArray/toJsonString methods,
	 * see {@link net.sf.cobolToJson.def.ICobolMultipleRecordsToArrayOut}
	 * 
	 * <pre>
     * Usage:
     * 
     *     CobolJsonConversion.newCobolJsonConversion(
     *                    new ReadCobolCopybook()
     *                        .setCopybookName("Test-Copybook")
     *                        .addFreeFormatCobolText(cobolCopybookCode)))
     *         .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
     *         .setFont("cp037")
     *
     *        .singleRecordToJsonObject()
     *            .setCobolRecord(line.getData())
     *            .toJsonString()
     * </pre           
	 *
	 * @param cobolCopybookDtls Cobol copybook details
	 * @return Cobol2Json builder
	 */
	public static ICobolJsonConversion newCobolJsonConversion(ICobolCopybookTextSource cobolCopybookDtls) {
		ICobolJsonConversion cobolJsonConversion = Cobol2JsonImp.newCobolJsonConversion(
				cobolCopybookDtls.getFreeFormatCopybookReader(), 
				cobolCopybookDtls.getCopybookName());
		cobolJsonConversion.setCopybookFileFormat(Cb2xmlConstants.FREE_FORMAT);
		return cobolJsonConversion;
	}
	
}
