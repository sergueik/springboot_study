package example;

import net.sf.JRecord.Common.RecordException;
import net.sf.JRecord.JRecordInterface1;
import net.sf.JRecord.def.IO.builders.ICobolIOBuilder;
import net.sf.JRecord.Details.LayoutDetail;
import net.sf.JRecord.Common.Constants;
import net.sf.JRecord.Numeric.ICopybookDialects;

import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.TestInstance;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.test.context.ActiveProfiles;

import java.util.stream.Stream;

import static org.junit.jupiter.api.Assertions.*;

import example.ExamplePropertiesRecord;

@TestInstance(TestInstance.Lifecycle.PER_CLASS)
@SpringBootTest
@ActiveProfiles({"test"})
class CopybookParserTest {

    @Autowired
    private ExamplePropertiesRecord examplePropertiesRecord;

    private ICobolIOBuilder iCobolIOBuilder = null;

    public final String[] badCopybooks() {

        final String baseBadPath = examplePropertiesRecord.baseBadPath();

        final String[] badCopybooks =
                examplePropertiesRecord.badCopybooks().toArray(new String[0]);

        String[] filePaths = new String[badCopybooks.length];

        for (int cnt = 0; cnt < badCopybooks.length; cnt++) {
            filePaths[cnt] = baseBadPath + badCopybooks[cnt];
        }

        return filePaths;
    }

    public Stream<String> goodCopybooks() {
        final String baseGoodPath = examplePropertiesRecord.baseGoodPath();
        return examplePropertiesRecord.goodCopybooks()
                .stream()
                .map(fileName -> baseGoodPath + fileName);
    }

    @DisplayName("Fails on Simple crafted Copybook With the number not88 Error")
    @ParameterizedTest
    @MethodSource("badCopybooks")
    void testBadCopybooks(final String copybookPath) {

        iCobolIOBuilder = JRecordInterface1.COBOL
                .newIOBuilder(copybookPath)
                .setDialect(ICopybookDialects.FMT_MAINFRAME)
                .setFileOrganization(Constants.IO_FIXED_LENGTH)
                .setFont("utf8");

        RecordException ex =
                assertThrows(RecordException.class, iCobolIOBuilder::getLayout);

        assertTrue(
                ex.getMessage().contains("number88"),
                "Expected parser to throw exception containing 'number88' but was: " + ex.getMessage()
        );
    }

    @DisplayName("Reads Copybook Without Error")
    @ParameterizedTest
    @MethodSource("goodCopybooks")
    void testGoodCopybooks(final String copybookPath) {

        iCobolIOBuilder = JRecordInterface1.COBOL
                .newIOBuilder(copybookPath)
                .setDialect(ICopybookDialects.FMT_MAINFRAME)
                .setFileOrganization(Constants.IO_FIXED_LENGTH)
                .setFont("utf8");

        LayoutDetail layoutDetail =
                assertDoesNotThrow(iCobolIOBuilder::getLayout);

        assertNotNull(
                layoutDetail,
                "Expected parser to read layout detail"
        );
    }
}
