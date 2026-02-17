package example;

import java.io.IOException;
import java.io.InputStream;
import java.nio.ByteBuffer;
import java.nio.channels.FileChannel;
import java.nio.file.Path;
import java.nio.file.StandardOpenOption;
import java.util.ArrayList;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.CompletableFuture;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

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

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import java.util.LinkedHashMap;
import java.util.Map;

public class LineProcessor {

    private static final Logger log = LoggerFactory.getLogger(LineProcessor.class);

    public void process(AbstractLine line, int chunkId) {

        Map<String, Object> record = new LinkedHashMap<>();

        LayoutDetail layoutDetail = line.getLayout();
        RecordDetail recordDetail = layoutDetail.getRecord(0);

        for (FieldDetail fieldDetail : recordDetail.getFields()) {
            String fieldName = fieldDetail.getName();
            Object value = line.getFieldValue(fieldName).asString().trim();
            record.put(fieldName, value);
        }

        log.info("Chunk {} Record: {}", chunkId, record);
    }
}
