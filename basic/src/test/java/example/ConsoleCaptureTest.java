package example;

import org.junit.Before;
import org.junit.Test;
import org.junit.Rule;

import org.springframework.boot.test.rule.OutputCapture;
// NOTE: fragile and may conflict with other tests
// see also:
// https://www.codota.com/code/java/classes/org.springframework.boot.test.rule.OutputCapture

import static org.hamcrest.Matchers.*;
import static org.junit.Assert.*;

public class ConsoleCaptureTest {
	@Rule
	public OutputCapture capture = new OutputCapture();

	@Test
	public void testName() throws Exception {
		System.out.println("Hello World!");
		assertThat(capture.toString(), containsString("World"));
	}

}
/*
java.lang.IllegalStateException: Found multiple @SpringBootConfiguration annotated classes 
[Generic bean: class [example.Application]; scope=; abstract=false; lazyInit=false; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; 
defined in file [C:\developer\sergueik\springboot_study\basic\target\classes\example\Application.class], 
Generic bean: class [example.ExampleApplication]; scope=; abstract=false; lazyInit=false; autowireMode=0; dependencyCheck=0; autowireCandidate=true; primary=false; factoryBeanName=null; factoryMethodName=null; initMethodName=null; destroyMethodName=null; defined in file [C:\developer\sergueik\springboot_study\basic\target\classes\example\ExampleApplication.class]]
	at org.springframework.util.Assert.state(Assert.java:70)
	at org.springframework.boot.test.context.SpringBootConfigurationFinder.scanPackage(SpringBootConfigurationFinder.java:69)
	at org.springframework.boot.test.context.SpringBootConfigurationFinder.findFromPackage(SpringBootConfigurationFinder.java:59)
	at org.springframework.boot.test.context.SpringBootConfigurationFinder.findFromClass(SpringBootConfigurationFinder.java:52)
	at org.springframework.boot.test.context.SpringBootTestContextBootstrapper.getOrFindConfigurationClasses(SpringBootTestContextBootstrapper.java:201)
	at org.springframework.boot.test.context.SpringBootTestContextBootstrapper.processMergedContextConfiguration(SpringBootTestContextBootstrapper.java:137)
	at org.springframework.boot.test.autoconfigure.web.servlet.WebMvcTestContextBootstrapper.processMergedContextConfiguration(WebMvcTestContextBootstrapper.java:35)
	at org.springframework.test.context.support.AbstractTestContextBootstrapper.buildMergedContextConfiguration(AbstractTestContextBootstrapper.java:409)
	at org.springframework.test.context.support.AbstractTestContextBootstrapper.buildDefaultMergedContextConfiguration(AbstractTestContextBootstrapper.java:323)
	at org.springframework.test.context.support.AbstractTestContextBootstrapper.buildMergedContextConfiguration(AbstractTestContextBootstrapper.java:277)
	at org.springframework.test.context.support.AbstractTestContextBootstrapper.buildTestContext(AbstractTestContextBootstrapper.java:112)
	at org.springframework.boot.test.context.SpringBootTestContextBootstrapper.buildTestContext(SpringBootTestContextBootstrapper.java:82)
	at org.springframework.test.context.TestContextManager.<init>(TestContextManager.java:120)
	at org.springframework.test.context.TestContextManager.<init>(TestContextManager.java:105)
	at org.springframework.test.context.junit4.SpringJUnit4ClassRunner.createTestContextManager(SpringJUnit4ClassRunner.java:152)
	at org.springframework.test.context.junit4.SpringJUnit4ClassRunner.<init>(SpringJUnit4ClassRunner.java:143)
	at org.springframework.test.context.junit4.SpringRunner.<init>(SpringRunner.java:49)
	at sun.reflect.NativeConstructorAccessorImpl.newInstance0(Native Method)
	
*/