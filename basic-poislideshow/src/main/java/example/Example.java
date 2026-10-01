package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */
import java.io.FileInputStream;
import java.io.IOException;
import java.util.Arrays;
import java.util.List;

import org.apache.poi.xslf.usermodel.XMLSlideShow;
import org.apache.poi.xslf.usermodel.XSLFNotes;
import org.apache.poi.xslf.usermodel.XSLFShape;
import org.apache.poi.xslf.usermodel.XSLFSlide;
import org.apache.poi.xslf.usermodel.XSLFTextParagraph;
import org.apache.poi.xslf.usermodel.XSLFTextShape;
import java.io.FileInputStream;

import org.apache.poi.xslf.usermodel.XMLSlideShow;
import org.apache.poi.xslf.usermodel.XSLFShape;
import org.apache.poi.xslf.usermodel.XSLFSlide;
import org.apache.poi.xslf.usermodel.XSLFTextShape;

import org.apache.commons.cli.CommandLine;
import org.apache.commons.cli.CommandLineParser;
import org.apache.commons.cli.DefaultParser;
import org.apache.commons.cli.MissingArgumentException;
import org.apache.commons.cli.Options;
import org.apache.commons.cli.ParseException;

public class Example {
	private final static Options options = new Options();
	private static CommandLineParser commandLineparser = new DefaultParser();
	private static CommandLine commandLine = null;
	private static boolean debug = false;
	private static String filename = null;
	
	public static void main(String[] args) throws Exception {
		options.addOption("h", "help", false, "Help");
		options.addOption("d", "debug", false, "Debug");
		options.addOption("f", "filename", true, "Filename");
		try {
			commandLine = commandLineparser.parse(options, args);
		} catch (MissingArgumentException e) {
			System.err.println("Aborting after exception " + e.toString());
			return;
		}
		if (commandLine.hasOption("h")) {
			help();
		}
		if (commandLine.hasOption("d")) {
			debug = true;
			System.err.println("filename: " + commandLine.getParsedOptionValue("filename") + "\n" + "arguments: "
					+ commandLine.getParsedOptionValue("arguments"));
			System.err.println(String.format("args: %s", commandLine.getArgList()));
			System.err.println("All optons: ");
			Arrays.asList(commandLine.getOptions()).stream().map(o -> o.getArgName() + " " + o.getValue()).forEach(System.err::println);
		}
		if (commandLine.hasOption("filename")) {
			filename = commandLine.getOptionValue("filename");
		}
		if (filename == null) {
			System.err.println("Missing required argument: filename");
			help();
			return;
		}

		try (FileInputStream fis = new FileInputStream(filename)) {

			XMLSlideShow ppt = new XMLSlideShow(fis);

			int slideNumber = 1;

			for (XSLFSlide slide : ppt.getSlides()) {

				System.out.println();
				System.out.println("===== SLIDE " + slideNumber + " =====");

				for (XSLFShape shape : slide.getShapes()) {

					if (shape instanceof XSLFTextShape) {

						XSLFTextShape textShape = (XSLFTextShape) shape;

						String text = textShape.getText();

						if (text != null && !text.trim().isEmpty()) {
							System.out.println(text.trim());
						}
					}
				}

				slideNumber++;
			}

			ppt.close();
		}
	}

	public static void help() {
		System.err.println(String.format("Usage: java %s --filename <filename>", "example.App"));
		System.exit(1);
	}

}
