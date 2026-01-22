package example;

import net.sf.JRecord.Details.AbstractLine;
import net.sf.JRecord.IO.AbstractLineWriter;
import net.sf.JRecord.IO.LineIOProvider;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Common.IFileStructureConstants;

import java.io.File;
import java.math.BigDecimal;

public class Generator {

    public static void main(String[] args) throws Exception {
        String copybookFile = "example.cbl";
        String outputFile = "sample.bin";

        // Create COBOL IO builder
        ICobolIOBuilder builder = JRecordInterface1.COBOL
                .newIOBuilder(copybookFile)
                .setFileOrganization(IFileStructureConstants.IO_FIXED_LENGTH)
                .setFont("cp037"); // EBCDIC

        // Create a new line
        AbstractLine line = builder.newLine();

        // Set fields by name (names must match the copybook)
        line.setField("CUSTOMER-ID", "ABC123");
        line.setField("NAME", "JOHN DOE");
        line.setField("ACCOUNT-NUMBER", new BigDecimal("123456789"));
        line.setField("BALANCE", new BigDecimal("1050.75"));

        // Get writer via LineIOProvider using file structure
        AbstractLineWriter writer = LineIOProvider.getInstance()
                .getLineWriter(IFileStructureConstants.IO_FIXED_LENGTH);

        // Write line to FileOutputStream
        writer.open(new java.io.FileOutputStream(outputFile));
        writer.write(line);
        writer.close();

        System.out.println("✅ EBCDIC row written to: " + outputFile);
    }
}
