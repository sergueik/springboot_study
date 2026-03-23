package io.github.kaluchi.jdtbridge;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Integration tests for edge cases: overloaded methods, inner classes,
 * enums, annotations, abstract classes.
 */
public class EdgeCaseIntegrationTest {

    private static final SearchHandler search = new SearchHandler();

    @BeforeClass
    public static void setUp() throws Exception {
        TestFixture.create();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        TestFixture.destroy();
    }

    // ---- Overloaded methods ----

    @Test
    public void typeInfoShowsAllOverloads() throws Exception {
        String json = search.handleTypeInfo(
                Map.of("class", "test.edge.Calculator"));
        // Should have 3 add methods
        int count = 0;
        int idx = 0;
        while ((idx = json.indexOf("\"name\":\"add\"", idx)) >= 0) {
            count++;
            idx++;
        }
        assertEquals("Should have 3 add() overloads", 3, count);
    }

    @Test
    public void sourceByMethodFindsAllOverloads() throws Exception {
        HttpServer.Response resp = search.handleSource(
                Map.of("class", "test.edge.Calculator", "method", "add"));
        // Without arity, all overloads should be returned
        String body = resp.body();
        assertTrue("Should contain int overload: " + body,
                body.contains("int add(int a, int b)"));
        assertTrue("Should contain double overload: " + body,
                body.contains("double add(double a, double b)"));
        assertTrue("Should contain 3-arg overload: " + body,
                body.contains("int add(int a, int b, int c)"));
    }

    @Test
    public void sourceByMethodWithArity() throws Exception {
        HttpServer.Response resp = search.handleSource(
                Map.of("class", "test.edge.Calculator",
                        "method", "add", "arity", "3"));
        String body = resp.body();
        assertTrue("Should contain 3-arg overload: " + body,
                body.contains("int a, int b, int c"));
        // Should be a single method, not multiple
        assertTrue("Should have start line header",
                resp.headers().containsKey("X-Start-Line"));
    }

    @Test
    public void referencesWithArity() throws Exception {
        // add(int, int) has arity 2
        String json = search.handleReferences(
                Map.of("class", "test.edge.Calculator",
                        "method", "add", "arity", "2"));
        // No external callers in test project
        assertEquals("[]", json);
    }

    // ---- Inner classes ----

    @Test
    public void findInnerClass() throws Exception {
        String json = search.handleFind(Map.of("name", "Inner"));
        assertTrue("Should find Outer.Inner: " + json,
                json.contains("test.edge.Outer.Inner")
                        || json.contains("Outer$Inner"));
    }

    @Test
    public void findStaticNested() throws Exception {
        String json = search.handleFind(Map.of("name", "StaticNested"));
        assertTrue("Should find StaticNested: " + json,
                json.contains("StaticNested"));
    }

    @Test
    public void typeInfoInnerClass() throws Exception {
        // Inner classes are found by $ separator in JDT
        String json = search.handleTypeInfo(
                Map.of("class", "test.edge.Outer"));
        assertTrue("Should be a class: " + json,
                json.contains("\"kind\":\"class\""));
        assertTrue("Should have name field: " + json,
                json.contains("\"name\":\"name\""));
    }

    @Test
    public void sourceOuter() throws Exception {
        HttpServer.Response resp = search.handleSource(
                Map.of("class", "test.edge.Outer"));
        assertTrue("Should contain Inner: " + resp.body(),
                resp.body().contains("public class Inner"));
        assertTrue("Should contain StaticNested: " + resp.body(),
                resp.body().contains("public static class StaticNested"));
    }

    // ---- Enum ----

    @Test
    public void typeInfoEnum() throws Exception {
        String json = search.handleTypeInfo(
                Map.of("class", "test.edge.Color"));
        assertTrue("Should be enum: " + json,
                json.contains("\"kind\":\"enum\""));
        assertTrue("Should have lower method: " + json,
                json.contains("\"name\":\"lower\""));
    }

    @Test
    public void sourceEnum() throws Exception {
        HttpServer.Response resp = search.handleSource(
                Map.of("class", "test.edge.Color"));
        assertTrue("Should contain RED: " + resp.body(),
                resp.body().contains("RED"));
    }

    @Test
    public void findEnum() throws Exception {
        String json = search.handleFind(Map.of("name", "Color"));
        assertTrue("Should find Color: " + json,
                json.contains("test.edge.Color"));
    }

    // ---- Annotation ----

    @Test
    public void typeInfoAnnotation() throws Exception {
        String json = search.handleTypeInfo(
                Map.of("class", "test.edge.Marker"));
        assertTrue("Should be annotation: " + json,
                json.contains("\"kind\":\"annotation\""));
    }

    @Test
    public void sourceAnnotation() throws Exception {
        HttpServer.Response resp = search.handleSource(
                Map.of("class", "test.edge.Marker"));
        assertTrue("Should contain @Retention: " + resp.body(),
                resp.body().contains("@Retention"));
        assertTrue("Should contain value(): " + resp.body(),
                resp.body().contains("String value()"));
    }

    // ---- Abstract class + deeper hierarchy ----

    @Test
    public void subtypesOfAbstract() throws Exception {
        String json = search.handleSubtypes(
                Map.of("class", "test.edge.AbstractPet"));
        assertTrue("Should find Parrot: " + json,
                json.contains("test.edge.Parrot"));
    }

    @Test
    public void hierarchyOfParrot() throws Exception {
        String json = search.handleHierarchy(
                Map.of("class", "test.edge.Parrot"));
        // Parrot → AbstractPet → Object
        assertTrue("Should have AbstractPet in supers: " + json,
                json.contains("test.edge.AbstractPet"));
        assertTrue("Should have Object in supers: " + json,
                json.contains("java.lang.Object"));
        // Parrot implements Animal (through AbstractPet)
        assertTrue("Should have Animal in interfaces: " + json,
                json.contains("test.model.Animal"));
    }

    @Test
    public void deepSubtypesOfAnimal() throws Exception {
        // Animal → Dog, Cat, AbstractPet, Parrot
        String json = search.handleSubtypes(
                Map.of("class", "test.model.Animal"));
        assertTrue("Should find Dog: " + json,
                json.contains("test.model.Dog"));
        assertTrue("Should find Cat: " + json,
                json.contains("test.model.Cat"));
        assertTrue("Should find AbstractPet: " + json,
                json.contains("test.edge.AbstractPet"));
        assertTrue("Should find Parrot: " + json,
                json.contains("test.edge.Parrot"));
    }

    @Test
    public void implementorsIncludesDeepHierarchy() throws Exception {
        // name() is declared in Animal, implemented by Dog, Cat,
        // AbstractPet, and Parrot inherits it
        String json = search.handleImplementors(
                Map.of("class", "test.model.Animal", "method", "name"));
        assertTrue("Should find Dog: " + json,
                json.contains("test.model.Dog"));
        assertTrue("Should find Cat: " + json,
                json.contains("test.model.Cat"));
        assertTrue("Should find AbstractPet: " + json,
                json.contains("test.edge.AbstractPet"));
    }

    // ---- Project info with edge types ----

    @Test
    public void projectInfoIncludesEdgePackage() throws Exception {
        ProjectHandler handler = new ProjectHandler();
        String json = handler.handleProjectInfo(
                Map.of("project", TestFixture.PROJECT_NAME));
        assertTrue("Should have test.edge: " + json,
                json.contains("test.edge"));
        assertTrue("Should have Calculator: " + json,
                json.contains("Calculator"));
        assertTrue("Should have Color as enum: " + json,
                json.contains("\"kind\":\"enum\""));
        assertTrue("Should have Marker as annotation: " + json,
                json.contains("\"kind\":\"annotation\""));
    }
}
