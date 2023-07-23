package example;

import java.io.ByteArrayInputStream;
import java.io.InputStream;

import org.jopendocument.dom.ODPackage;
import org.jopendocument.dom.spreadsheet.SpreadSheet;

import edu.emory.mathcs.backport.java.util.Arrays;

public class ODS {
	public final SpreadSheet spreadSheet;
	public final ODPackage oDPackage;

	public ODS(byte[] content) {

		try (InputStream inputStream = new ByteArrayInputStream(content)) {
			oDPackage = new ODPackage(inputStream);
			spreadSheet = SpreadSheet.get(oDPackage);
			System.err.println("Version: " + oDPackage.getVersion().getManifest()
					+ " " + oDPackage.getClass().getPackage().getName() + " "
					+ oDPackage.getClass().getPackage().getImplementationVersion());
		} catch (Exception e) {
			throw new IllegalArgumentException(" Exception: " + e.toString());
		}
	}

}
