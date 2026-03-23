package io.github.kaluchi.jdtbridge;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Integration tests for SearchHandler using a real JDT workspace.
 * Creates a test project with known classes, then verifies search results.
 */
public class SearchIntegrationTest {

    private static final SearchHandler handler = new SearchHandler();

    @BeforeClass
    public static void setUp() throws Exception {
        TestFixture.create();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        TestFixture.destroy();
    }

    // ---- /find ----

    @Test
    public void findByExactName() throws Exception {
        String json = handler.handleFind(Map.of("name", "Animal"));
        assertTrue("Should find Animal: " + json,
                json.contains("test.model.Animal"));
    }

    @Test
    public void findByPattern() throws Exception {
        String json = handler.handleFind(Map.of("name", "*Service"));
        assertTrue("Should find AnimalService: " + json,
                json.contains("test.service.AnimalService"));
    }

    @Test
    public void findSourceOnly() throws Exception {
        String json = handler.handleFind(
                Map.of("name", "Dog", "source", ""));
        assertTrue("Should find source Dog: " + json,
                json.contains("test.model.Dog"));
        // Should not include binary JDK types
        assertFalse("Should not contain binary: " + json,
                json.contains("binary"));
    }

    @Test
    public void findMissingParam() throws Exception {
        String json = handler.handleFind(Map.of());
        assertTrue("Should return error: " + json,
                json.contains("error"));
    }

    @Test
    public void findNonExistent() throws Exception {
        String json = handler.handleFind(
                Map.of("name", "NoSuchTypeXYZ"));
        assertEquals("[]", json);
    }

    // ---- /subtypes ----

    @Test
    public void subtypesOfInterface() throws Exception {
        String json = handler.handleSubtypes(
                Map.of("class", "test.model.Animal"));
        assertTrue("Should find Dog: " + json,
                json.contains("test.model.Dog"));
        assertTrue("Should find Cat: " + json,
                json.contains("test.model.Cat"));
    }

    @Test
    public void subtypesOfClass() throws Exception {
        String json = handler.handleSubtypes(
                Map.of("class", "test.model.Dog"));
        assertEquals("Dog has no subtypes", "[]", json);
    }

    @Test
    public void subtypesNotFound() throws Exception {
        String json = handler.handleSubtypes(
                Map.of("class", "no.such.Type"));
        assertTrue("Should return error: " + json,
                json.contains("error"));
    }

    // ---- /hierarchy ----

    @Test
    public void hierarchyOfDog() throws Exception {
        String json = handler.handleHierarchy(
                Map.of("class", "test.model.Dog"));
        // Dog extends Object
        assertTrue("Should have Object in supers: " + json,
                json.contains("java.lang.Object"));
        // Dog implements Animal
        assertTrue("Should have Animal in interfaces: " + json,
                json.contains("test.model.Animal"));
        // Dog has no subtypes
        assertTrue("Should have empty subtypes: " + json,
                json.contains("\"subtypes\":[]"));
    }

    @Test
    public void hierarchyOfAnimal() throws Exception {
        String json = handler.handleHierarchy(
                Map.of("class", "test.model.Animal"));
        assertTrue("Should have Dog in subtypes: " + json,
                json.contains("test.model.Dog"));
        assertTrue("Should have Cat in subtypes: " + json,
                json.contains("test.model.Cat"));
    }

    // ---- /references ----

    @Test
    public void referencesToType() throws Exception {
        String json = handler.handleReferences(
                Map.of("class", "test.model.Dog"));
        assertTrue("Should find ref in AnimalService: " + json,
                json.contains("AnimalService"));
    }

    @Test
    public void referencesToMethod() throws Exception {
        String json = handler.handleReferences(
                Map.of("class", "test.model.Dog", "method", "bark"));
        assertTrue("Should find bark() ref: " + json,
                json.contains("AnimalService"));
    }

    @Test
    public void referencesToField() throws Exception {
        String json = handler.handleReferences(
                Map.of("class", "test.model.Dog", "field", "age"));
        // age is private, no external references
        assertEquals("[]", json);
    }

    @Test
    public void referencesMethodNotFound() throws Exception {
        String json = handler.handleReferences(
                Map.of("class", "test.model.Dog", "method", "fly"));
        assertTrue("Should return error: " + json,
                json.contains("error"));
    }

    // ---- /implementors ----

    @Test
    public void implementorsOfInterfaceMethod() throws Exception {
        String json = handler.handleImplementors(
                Map.of("class", "test.model.Animal", "method", "name"));
        assertTrue("Should find Dog.name: " + json,
                json.contains("test.model.Dog"));
        assertTrue("Should find Cat.name: " + json,
                json.contains("test.model.Cat"));
    }

    // ---- /type-info ----

    @Test
    public void typeInfoClass() throws Exception {
        String json = handler.handleTypeInfo(
                Map.of("class", "test.model.Dog"));
        assertTrue("Should be class: " + json,
                json.contains("\"kind\":\"class\""));
        assertTrue("Should have name method: " + json,
                json.contains("\"name\":\"name\""));
        assertTrue("Should have bark method: " + json,
                json.contains("\"name\":\"bark\""));
        assertTrue("Should have age field: " + json,
                json.contains("\"name\":\"age\""));
        assertTrue("Should implement Animal: " + json,
                json.contains("Animal"));
    }

    @Test
    public void typeInfoInterface() throws Exception {
        String json = handler.handleTypeInfo(
                Map.of("class", "test.model.Animal"));
        assertTrue("Should be interface: " + json,
                json.contains("\"kind\":\"interface\""));
    }

    // ---- /source ----

    @Test
    public void sourceFullClass() throws Exception {
        HttpServer.Response resp = handler.handleSource(
                Map.of("class", "test.model.Dog"));
        assertEquals("text/plain", resp.contentType());
        assertTrue("Should contain class body: " + resp.body(),
                resp.body().contains("public class Dog"));
        assertTrue("Should contain bark method: " + resp.body(),
                resp.body().contains("public void bark()"));
    }

    @Test
    public void sourceMethod() throws Exception {
        HttpServer.Response resp = handler.handleSource(
                Map.of("class", "test.model.Dog", "method", "bark"));
        assertEquals("text/plain", resp.contentType());
        assertTrue("Should contain bark: " + resp.body(),
                resp.body().contains("bark"));
        assertTrue("Should have line header",
                resp.headers().containsKey("X-Start-Line"));
    }

    @Test
    public void sourceNotFound() throws Exception {
        HttpServer.Response resp = handler.handleSource(
                Map.of("class", "no.such.Type"));
        assertTrue("Should be error JSON: " + resp.body(),
                resp.body().contains("error"));
    }

    // ---- /projects ----

    @Test
    public void projectsIncludesTestProject() throws Exception {
        String json = handler.handleProjects();
        assertTrue("Should include test project: " + json,
                json.contains(TestFixture.PROJECT_NAME));
    }
}
