package net.sf.cobolToJson.zTest.cobolJsonConversion;

import static org.junit.Assert.*;

import java.io.IOException;

import org.junit.Test;

public class TestJsonToCobolConversion {

	@Test
	public void testArray() throws IOException {
		RunJsonToCobolConversionTests runArrayTest = 
				new RunJsonToCobolConversionTests(
						new CobolJsonConversionDetails(
								new ArrayCopybookDetails.SimpleArray()));
		runArrayTest.testSingleObject();
		runArrayTest.testMultipleRecords();
	}

	@Test
	public void test2dimensionArray() throws IOException {
		RunJsonToCobolConversionTests runArrayTest = 
				new RunJsonToCobolConversionTests(
						new CobolJsonConversionDetails(
								new ArrayCopybookDetails.TwoDimensionalArray()));
		runArrayTest.testSingleObject();
		runArrayTest.testMultipleRecords();
	}

	@Test
	public void testComplexArray() throws IOException {
		RunJsonToCobolConversionTests runArrayTest = 
				new RunJsonToCobolConversionTests(
						new CobolJsonConversionDetails(
								new ArrayCopybookDetails.ComplexArray()));
		runArrayTest.testSingleObject();
		runArrayTest.testMultipleRecords();
	}


	@Test
	public void testArrayC2J() throws IOException {
		RunJsonToCobolConversionTests runArrayTest = 
				new RunJsonToCobolConversionTests(
						new Cobol2JsonDetails(
								new ArrayCopybookDetails.SimpleArray()));
		runArrayTest.testSingleObject();
		runArrayTest.testMultipleRecords();
	}

	@Test
	public void test2dimensionArrayC2J() throws IOException {
		RunJsonToCobolConversionTests runArrayTest = 
				new RunJsonToCobolConversionTests(
						new Cobol2JsonDetails(
								new ArrayCopybookDetails.TwoDimensionalArray()));
		runArrayTest.testSingleObject();
		runArrayTest.testMultipleRecords();
	}

	@Test
	public void testComplexArrayC2J() throws IOException {
		RunJsonToCobolConversionTests runArrayTest = 
				new RunJsonToCobolConversionTests(
						new Cobol2JsonDetails(
								new ArrayCopybookDetails.ComplexArray()));
		runArrayTest.testSingleObject();
		runArrayTest.testMultipleRecords();
	}

}
