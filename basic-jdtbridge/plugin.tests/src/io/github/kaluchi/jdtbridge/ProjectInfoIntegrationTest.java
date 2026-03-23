package io.github.kaluchi.jdtbridge;

import static org.junit.Assert.assertTrue;

import java.util.Map;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import org.junit.Test;

/**
 * Integration tests for ProjectHandler using a real JDT workspace.
 */
public class ProjectInfoIntegrationTest {

    private static final ProjectHandler handler = new ProjectHandler();

    @BeforeClass
    public static void setUp() throws Exception {
        TestFixture.create();
    }

    @AfterClass
    public static void tearDown() throws Exception {
        TestFixture.destroy();
    }

    @Test
    public void projectInfoBasicFields() throws Exception {
        String json = handler.handleProjectInfo(
                Map.of("project", TestFixture.PROJECT_NAME));
        assertTrue("Should have name: " + json,
                json.contains("\"name\":\"" + TestFixture.PROJECT_NAME + "\""));
        assertTrue("Should have totalTypes: " + json,
                json.contains("\"totalTypes\":"));
        assertTrue("Should have sourceRoots: " + json,
                json.contains("\"sourceRoots\":["));
    }

    @Test
    public void projectInfoIncludesPackages() throws Exception {
        String json = handler.handleProjectInfo(
                Map.of("project", TestFixture.PROJECT_NAME));
        assertTrue("Should have test.model: " + json,
                json.contains("test.model"));
        assertTrue("Should have test.service: " + json,
                json.contains("test.service"));
        assertTrue("Should have test.broken: " + json,
                json.contains("test.broken"));
    }

    @Test
    public void projectInfoIncludesTypes() throws Exception {
        String json = handler.handleProjectInfo(
                Map.of("project", TestFixture.PROJECT_NAME));
        assertTrue("Should have Animal: " + json,
                json.contains("Animal"));
        assertTrue("Should have Dog: " + json,
                json.contains("Dog"));
        assertTrue("Should have AnimalService: " + json,
                json.contains("AnimalService"));
    }

    @Test
    public void projectInfoMembersIncluded() throws Exception {
        // Small project — members should be included
        String json = handler.handleProjectInfo(
                Map.of("project", TestFixture.PROJECT_NAME));
        assertTrue("Should include members: " + json,
                json.contains("\"membersIncluded\":true"));
        // With members, methods should be an object with visibility groups
        assertTrue("Should have public methods: " + json,
                json.contains("\"public\":["));
    }

    @Test
    public void projectInfoNotFound() throws Exception {
        String json = handler.handleProjectInfo(
                Map.of("project", "nonexistent-xyz"));
        assertTrue("Should return error: " + json,
                json.contains("error"));
    }

    @Test
    public void projectInfoHasJavaNature() throws Exception {
        String json = handler.handleProjectInfo(
                Map.of("project", TestFixture.PROJECT_NAME));
        assertTrue("Should have java nature: " + json,
                json.contains("\"java\""));
    }
}
