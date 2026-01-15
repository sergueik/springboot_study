package net.sf.cobolToJson.zTest.json2cbl;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

import net.sf.JRecord.Common.Conversion;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.External.CopybookLoader;
import net.sf.JRecord.ExternalRecordSelection.ExternalFieldSelection;
import net.sf.JRecord.Option.IReformatFieldNames;
import net.sf.cobolToJson.def.ICobol2Json;

public class TstCblToJsonFlatten01 {

	private final  static ICopyBookTestData[] TEST_COPYBOOKS = {			
			new CopyBookTestData("DTAR020", IFileStructureConstants.IO_FIXED_LENGTH, false, true)
					.setDataFileName(Cbl2JsonCode.getFullName("DTAR020_tst1.bin"))
					.setJsonData(Cbl2JsonCode.getFullName("json/" + "DTAR020_tst1F.json")),

			new AmsPoTestData(false)
							.setDataFileName(Cbl2JsonCode.getFullName("Ams_PODownload_20041231.txt")),
			new AmsPoTestData("amsPoDownloadGroup2", false)
							.setDataFileName(Cbl2JsonCode.getFullName("Ams_PODownload_20041231_small.txt")),
			new AmsPoTestData("amsPoDownloadGroup2", true)
							.setDataFileName(Cbl2JsonCode.getFullName("Ams_PODownload_20041231_small.txt")),
	};
	@Test
	public void test() throws IOException {
		for (ICopyBookTestData d : TEST_COPYBOOKS) {
			System.out.println(d.getCopybookName());
			
			String expected = d.readExpectedJsonFile();
			String data2json = d.data2json(IReformatFieldNames.RO_LEAVE_ASIS);
			if (! expected.equals(data2json)) {
				System.out.println(expected.substring(0, Math.min(20, expected.length())).replace('\n', ' '));
				System.out.println(data2json.substring(0, Math.min(20, data2json.length())).replace('\n', ' '));
				assertEquals(d.getCopybookName(),
						expected, data2json);
			}
		}
	}

	@Test
	public void test1() throws IOException {
		CopyBookTestData d = new AmsPoTestData(true)
				.setFontName("")
//				.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
				.setDataFileName(Cbl2JsonCode.getFullName("Ams_PODownload_20041231.txt"))
				.setJsonData(Cbl2JsonCode.getFullName("json/" + "amsPoDownload.json"));
//		System.out.println(d.data2json(IReformatFieldNames.RO_LEAVE_ASIS));		
		String expected = d.readExpectedJsonFile();
		String data2json = d.data2json(IReformatFieldNames.RO_UNDERSCORE);
		data2json = Conversion.replace(data2json, "amsPoDownloadGroup", "amsPoDownload").toString();
		if (! expected.equals(data2json)) {
			System.out.println(expected.substring(0, 20).replace('\n', ' '));
			System.out.println(data2json.substring(0, 20).replace('\n', ' '));
			assertEquals(d.copybookFileName,
					expected, data2json);
		}


	}
	
	private static class AmsPoTestData extends CopyBookTestData {
		AmsPoTestData(boolean flatten) {
			this("amsPoDownloadGroup", flatten);
		}
		AmsPoTestData(String copybook, boolean flatten) {
			super(copybook, IFileStructureConstants.IO_BIN_TEXT, false, flatten);
			super.setFontName("");
		}

		@Override
		protected ICobol2Json createCobol2Json(int tagFormat) {
			
			return super.createCobol2Json(tagFormat)
						.setSplitCopybook(CopybookLoader.SPLIT_01_LEVEL)
					    .setRecordSelection("PO-Record", newFieldSelection("Record-Type","H1"))
					    .setRecordSelection("Product-Record", newFieldSelection("Record-Type","D1"))
					    .setRecordSelection("Location-Record", newFieldSelection("Record-Type","S1"));

		}


	    private  ExternalFieldSelection newFieldSelection(String fieldName, String value) {
	    	ExternalFieldSelection r = new ExternalFieldSelection(fieldName, value);
	    	r.setCaseSensitive(false);
	    	return r;
	    }
		
		
	}
}
