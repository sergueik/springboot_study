package io.github.kaluchi.jdtbridge;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Integration tests for DiagnosticsHandler using a real JDT workspace.
 * The test project has a BrokenClass with an intentional compilation error.
 */
public class DiagnosticsIntegrationTest {

    private static final DiagnosticsHandler handler =
            new DiagnosticsHandler();

    @BeforeClass
    public static void setUp() throws Exception {
        TestFixture.create();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        TestFixture.destroy();
    }

    @Test
    public void errorsFindsCompilationError() throws Exception {
        Map<String, String> params = new HashMap<>();
        params.put("project", TestFixture.PROJECT_NAME);
        params.put("no-refresh", "");
        String json = handler.handleErrors(params);
        assertTrue("Should find error in BrokenClass: " + json,
                json.contains("BrokenClass"));
        assertTrue("Should be ERROR severity: " + json,
                json.contains("\"severity\":\"ERROR\""));
        assertTrue("Should mention UnknownType: " + json,
                json.contains("UnknownType"));
    }

    @Test
    public void errorsCleanProject() throws Exception {
        // Filter by file that has no errors
        Map<String, String> params = new HashMap<>();
        params.put("file",
                "/" + TestFixture.PROJECT_NAME + "/src/test/model/Dog.java");
        params.put("no-refresh", "");
        String json = handler.handleErrors(params);
        assertEquals("Dog.java should have no errors", "[]", json);
    }

    @Test
    public void errorsWithWarnings() throws Exception {
        Map<String, String> params = new HashMap<>();
        params.put("project", TestFixture.PROJECT_NAME);
        params.put("warnings", "");
        params.put("no-refresh", "");
        String json = handler.handleErrors(params);
        // Should at least find the ERROR
        assertTrue("Should find errors: " + json,
                json.contains("ERROR"));
    }

    @Test
    public void errorsProjectNotFound() throws Exception {
        Map<String, String> params = new HashMap<>();
        params.put("project", "no-such-project-xyz");
        params.put("no-refresh", "");
        String json = handler.handleErrors(params);
        assertTrue("Should return error: " + json,
                json.contains("error"));
    }
}
