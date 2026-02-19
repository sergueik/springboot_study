package example;

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
import net.sf.JRecord.Common.FieldDetail;
import net.sf.JRecord.Common.IFileStructureConstants;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Common.IFileStructureConstants;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.io.InputStream;
import java.io.Reader;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;

public class ChunkWorker implements Runnable {

	private static final Logger log = LoggerFactory.getLogger(ChunkWorker.class);

	private final Path copybookFile;
	private final Path inputFile;
	private final FileChunk chunk;
	private final LineProcessor lineProcessor;
	private final String font;

	public ChunkWorker(Path copybookFile, Path inputFile, FileChunk chunk, LineProcessor lineProcessor, String font) {
		log.info("instantiate ChunkWorker with {} {} {} {} ", copybookFile.toString(), inputFile.toString(),
				chunk.getId(), font);
		this.copybookFile = copybookFile;
		this.inputFile = inputFile;
		this.chunk = chunk;
		this.lineProcessor = lineProcessor;
		this.font = font;
	}

	@Override
	public void run() {

		// NOTE: in In JRecordInterface1.COBOL.newIOBuilder(fileName), the fileName
		// parameter means:
		//
		// the COBOL copybook file
		// not the binary data file
		log.info("instantiate Cobol IO builder with {} ", copybookFile.toString());
		try {
			Thread.sleep(1000);
		} catch (InterruptedException e) {
		}

		ICobolIOBuilder ioBuilder = JRecordInterface1.COBOL.newIOBuilder(copybookFile.toString())
				.setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH).setFont(font);

		FileChannel fileChannel = null;
		InputStream inputStream = null;
		AbstractLineReader reader = null;
		log.info("Processing chunk {} [{} - {}]", chunk.getId(), chunk.getStartOffset(), chunk.getEndOffset());

		try {
			fileChannel = FileChannel.open(inputFile, StandardOpenOption.READ);

			inputStream = new ChunkStream(fileChannel, chunk.getStartOffset(), chunk.getEndOffset());

			reader = ioBuilder.newReader(inputStream);

			AbstractLine line;
			while ((line = reader.read()) != null) {
				lineProcessor.process(line, chunk.getId());
			}

		} catch (Exception e) {
			throw new RuntimeException("Chunk " + chunk.getId() + " failed", e);

		} finally {
			// close reader first
			if (reader != null) {
				try {
					reader.close();
				} catch (Exception ex) {
					log.warn("Failed to close reader for chunk {}", chunk.getId(), ex);
				}
			}

			if (inputStream != null) {
				try {
					inputStream.close();
				} catch (Exception ex) {
					log.warn("Failed to close inputStream for chunk {}", chunk.getId(), ex);
				}
			}

			if (fileChannel != null) {
				try {
					fileChannel.close();
				} catch (Exception ex) {
					log.warn("Failed to close fileChannel for chunk {}", chunk.getId(), ex);
				}
			}
		}

	}

}