package example;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;
import example.FileChunk;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Details.RecordDetail;
import net.sf.JRecord.Details.fieldValue.IFieldValue;
import net.sf.JRecord.IO.AbstractLineReader;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFileStructureConstants;

public class ChunkPartitioner {
	private final String copybookPath;
	private long fileSize;
	private long recordSize;
	private final long MIN_RECORDS = 10L;
	private final long approxChunkSize;
	private ICobolIOBuilder iCobolIOBuilder;

	public ChunkPartitioner(final String copybookPath, final long fileSize, final long approxChunkSize) {
		this.copybookPath = copybookPath;
		if (fileSize <= 0) {
			throw new IllegalArgumentException("fileSize must be > 0");
		}

		if (approxChunkSize <= 0) {
			throw new IllegalArgumentException("approxChunkSize must be > 0");
		}
		this.fileSize = fileSize;
		this.approxChunkSize = approxChunkSize;
	}

	List<FileChunk> partitionFile() throws IOException {

		List<FileChunk> chunks = new ArrayList<>();
		iCobolIOBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookPath)
				.setFileOrganization(Constants.IO_FIXED_LENGTH);
		LayoutDetail layoutDetail = iCobolIOBuilder.getLayout();
		if (layoutDetail.getRecordCount() == 0) {
			throw new IllegalStateException("Copybook defines no records");
		}
		recordSize = layoutDetail.getRecord(0).getLength();
		long numRecords = approxChunkSize / recordSize;
		long chunkSize = numRecords * recordSize;
		if (numRecords <= MIN_RECORDS) {
			throw new IllegalArgumentException("numRecords must be > " + MIN_RECORDS);
		}
		long offset = 0;
		int id = 0;

		while (offset < fileSize) {
			long size = Math.min(chunkSize, fileSize - offset);
			chunks.add(new FileChunk(id++, offset, offset + size));
			offset += size;
		}

		return chunks;
	}
}
