package io.github.kaluchi.jdtbridge;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.HashMap;
import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Tests for TestHandler: error cases and utility methods.
 *
 * Note: actually running JUnit tests via TestHandler requires the test
 * project to have JUnit on its classpath, which is complex to set up
 * in a PDE test environment. These tests verify the handler's error
 * handling and parameter validation.
 */
public class TestHandlerTest {

    private static final TestHandler handler = new TestHandler();

    @BeforeClass
    public static void setUp() throws Exception {
        TestFixture.create();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        TestFixture.destroy();
    }

    @Test
    public void missingParams() throws Exception {
        String json = handler.handleTest(Map.of());
        assertTrue("Should return error: " + json,
                json.contains("error"));
        assertTrue("Should mention missing param: " + json,
                json.contains("Missing"));
    }

    @Test
    public void typeNotFound() throws Exception {
        Map<String, String> params = new HashMap<>();
        params.put("class", "no.such.TestClass");
        params.put("no-refresh", "");
        String json = handler.handleTest(params);
        assertTrue("Should return error: " + json,
                json.contains("error"));
        assertTrue("Should mention not found: " + json,
                json.contains("not found"));
    }

    @Test
    public void projectNotFound() throws Exception {
        Map<String, String> params = new HashMap<>();
        params.put("project", "nonexistent-project-xyz");
        params.put("no-refresh", "");
        String json = handler.handleTest(params);
        assertTrue("Should return error: " + json,
                json.contains("error"));
    }

    @Test
    public void detectTestKindJunit4() throws Exception {
        // test.model.Dog has no JUnit 5 on classpath → JUnit 4
        Object type = invokeFindType("test.model.Dog");
        String kind = invokeDetectTestKind(type);
        assertEquals("org.eclipse.jdt.junit.loader.junit4", kind);
    }

    @Test
    public void parseTimeoutDefault() {
        assertEquals(120, invokeParseTimeout(null, 120));
    }

    @Test
    public void parseTimeoutValid() {
        assertEquals(30, invokeParseTimeout("30", 120));
    }

    @Test
    public void parseTimeoutInvalid() {
        assertEquals(120, invokeParseTimeout("abc", 120));
    }

    // ---- Reflection helpers ----

    private Object invokeFindType(String fqn) {
        try {
            Class<?> clazz = Class.forName(
                    "io.github.kaluchi.jdtbridge.JdtUtils");
            var method = clazz
                    .getDeclaredMethod("findType", String.class);
            method.setAccessible(true);
            return method.invoke(null, fqn);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private String invokeDetectTestKind(Object type) {
        try {
            var method = TestHandler.class
                    .getDeclaredMethod("detectTestKind",
                            org.eclipse.jdt.core.IType.class);
            method.setAccessible(true);
            return (String) method.invoke(handler, type);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }

    private int invokeParseTimeout(String s, int defaultVal) {
        try {
            var method = TestHandler.class
                    .getDeclaredMethod("parseTimeout",
                            String.class, int.class);
            method.setAccessible(true);
            return (int) method.invoke(handler, s, defaultVal);
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
