package example.utils;

import java.io.Closeable;
import java.io.IOException;
import java.nio.file.Path;
import java.util.LinkedHashMap;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.Line;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;

public class CopybookBatchReader implements Closeable {
	private static final Logger log = LoggerFactory.getLogger(CopybookBatchReader.class);

	private AbstractLineReader reader;
	private LayoutDetail layout;
	private long recordSize;

	public CopybookBatchReader(Path copybookFile, Path dataFile, String page) throws Exception {

		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookFile.toString())
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH).setFont(page); // EBCDIC

		/*
		 * ICobolIOBuilder ioBuilder = CopybookLoaderFactory.getInstance()
		 * .getCobolIOBuilder(copybookFile.toString())
		 * .setFileOrganization(Constants.IO_FIXED_LENGTH) .setFont("cp037");
		 */
		// NOTE: highly unstable"
		LayoutDetail layoutDetail = ioBuilder.getLayout();
		if (layoutDetail.getRecordCount() == 0) {
			throw new IllegalStateException(String.format("Copybook %s defines no records", copybookFile.toString()));
		}
		recordSize = layoutDetail.getRecord(0).getLength();		
		log.info("record size = {}", recordSize);
		RecordDetail recordDetail = layoutDetail.getRecord(0);
		for (FieldDetail fieldDetail : recordDetail.getFields()) {
			String fieldName = fieldDetail.getName();
			log.info("record field = {}", fieldName);
		}
		
		reader = ioBuilder.newReader(dataFile.toString());
	}

	public Map<String, Object> readOne() throws Exception {
		AbstractLine line = reader.read();
		if (line == null)
			return null;

		Map<String, Object> record = new LinkedHashMap<>();
		LayoutDetail layoutDetail = line.getLayout();
		RecordDetail recordDetail = layoutDetail.getRecord(0);
		for (FieldDetail fieldDetail : recordDetail.getFields()) {
			String fieldName = fieldDetail.getName();
			IFieldValue fieldValue = line.getFieldValue(fieldName);
			// NOTE: need to use asObject()
			Object value = fieldValue.asString().trim();
			record.put(fieldName, value);
		}
		return record;
	}

	@Override
	public void close() throws IOException {
		reader.close();
	}
}
