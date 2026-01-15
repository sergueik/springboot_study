package net.sf.JRecord.cbl2json.zTest.json2cbl;

import java.io.FileInputStream;
import java.io.IOException;
import java.io.StringWriter;

import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Numeric.ICopybookDialects;
import net.sf.cobolToJson.def.ICobol2Json;
import net.sf.cobolToJson.impl.Cobol2JsonImp;

public class CopyBookTestData implements ICopyBookTestData {
	public final String copybookFileName, copybookName, defaultFileName, defaultJsonData;
	private String dataFileName, jsonData, fontName = "cp037";
	public final boolean dropCopybookName, flatten;
	private boolean prettyPrint = true;
	private final int fileOrganisation;
//	private int splitCopybook = CopybookLoader.SPLIT_NONE;
	
	public CopyBookTestData(String copybookName) {
		this(copybookName, IFileStructureConstants.IO_FIXED_LENGTH, false, false);
	}

	/**
	 * 
	 * @param copybookName
	 * @param fixed
	 * @param dropCopybookName
	 * @param flatten
	 */
	public CopyBookTestData(String copybookName, 
			int fileOrganisation, boolean dropCopybookName, boolean flatten) {
		super();
		
		this.copybookName = copybookName;
		this.copybookFileName = Cbl2JsonCode.getFullName("cobol/" + copybookName + ".cbl");
		this.defaultFileName = copybookName + ".bin";
		
		this.fileOrganisation  = fileOrganisation;
		this.dropCopybookName = dropCopybookName;
		this.flatten = flatten;
		
		StringBuilder ext = new StringBuilder(dropCopybookName ? "Y" : "N") ;
		if (flatten) {
			ext.append("F");
		}
		
		this.defaultJsonData = "json/" +  copybookName + "_Data" + ext + ".json";
		System.out.println(defaultJsonData);
	}


	@Override
	public String getCopybookName() {
		return copybookName;
	}

	public String getDataFileName() {
		return dataFileName;
	}


	public CopyBookTestData setDataFileName(String dataFileName) {
		this.dataFileName = dataFileName;
		return this;
	}


	@Override
	public String getJsonData() {
		return jsonData;
	}


	public CopyBookTestData setJsonData(String jsonData) {
		this.jsonData = jsonData;
		return this;
	}
	
	
	public CopyBookTestData setFontName(String fontname) {
		this.fontName = fontname;
		return this;
	}

	public CopyBookTestData setPrettyPrint(boolean prettyPrint) {
		this.prettyPrint = prettyPrint;
		return this;
	}

//	/**
//	 * 
//	 * @param splitCopybook Wether to Split the Copybook or not. Options are:<ul>
//	 *    <li><b>CopybookLoader.SPLIT_NONE</b> - No Split
//	 *    <li><b>CopybookLoader.SPLIT_REDEFINE</b> - Split on highest-Level redefines
//	 *    <li><b>CopybookLoader.SPLIT_01_LEVEL</b> - Split on 01 levels
//	 *    <li><b>CopybookLoader.SPLIT_HIGHEST_REPEATING</b> - Split on the highest group level
//	 *    in the copybook.
//	 * </ul>
//	 */
//	protected CopyBookTestData setSplitCopybook(int splitCopybook) {
//		this.splitCopybook = splitCopybook;
//		return this;
//	}

	@Override
	public String readExpectedJsonFile() throws IOException  {
		if (jsonData == null) {
			jsonData = Cbl2JsonCode.getFullName(defaultJsonData);
		}
		return Cbl2JsonCode.loadFile(jsonData, "\n", false);
	}

	@Override
	public String data2json(int tagFormat) 
	throws  IOException {
		
		StringWriter writer = new StringWriter(0x10000);
//		int fileOrg = fixed ? IFileStructureConstants.IO_FIXED_LENGTH : IFileStructureConstants.IO_VB;

		dataFileName = dataFileName == null ? Cbl2JsonCode.getFullName(defaultFileName) : dataFileName;
		createCobol2Json(tagFormat)
					  .cobol2json(new FileInputStream(dataFileName), writer);

	    return writer.toString();
	}

	protected ICobol2Json createCobol2Json(int tagFormat) {
		return Cobol2JsonImp.newCobol2Json(copybookFileName)
					  .setFileOrganization(fileOrganisation)
					  .setFlattenStructure(flatten)
					  .setFont(fontName)
//					  .setSplitCopybook(splitCopybook)
					  .setPrettyPrint(prettyPrint)
					  .setDialect(ICopybookDialects.FMT_MAINFRAME) // This is the default
					  .setTagFormat(tagFormat)
					  .setDropCopybookNameFromFields(dropCopybookName);
	}
}
