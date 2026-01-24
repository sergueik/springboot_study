package example;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.core.IsNot.not;
import static org.hamcrest.CoreMatchers.notNullValue;

import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.io.*;

import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;

import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.stream.Stream;

import com.jayway.jsonpath.JsonPath;

public class IntegrationTest {

	private static Path tempDir = null;
	private static Path inputBin = null;
	private static Path copybookFile = null;
	private static Path outputJson = null;

	@BeforeAll
	public static void setup() throws IOException {
		tempDir = Files.createTempDirectory("coboltest");
		inputBin = tempDir.resolve("input.bin");
		copybookFile = tempDir.resolve("copybook.cpy");
		outputJson = tempDir.resolve("output.json");
	}

	@DisplayName("Parse a binary file and a copybook, examine the fields")
	@ParameterizedTest
	@MethodSource("testData")
	public void test1(String hexRow, String copybookText, String jsonPath, Object value) throws Exception {

		Files.write(inputBin, hexStringToByteArray(hexRow));
		Files.writeString(copybookFile, copybookText, StandardCharsets.US_ASCII);

		runProcess(inputBin, copybookFile, outputJson);
		// TODO: exceptions
		String json = Files.readString(outputJson, StandardCharsets.UTF_8);

		Object actual = JsonPath.read(json, jsonPath);
		assertThat(actual, is(value));
	}

	@Disabled
	@DisplayName("Parse a binary file and a copybook, examine the fields")
	@ParameterizedTest
	@MethodSource("testData2")
	public void test2(String hexRow, String copybookText,  Map<String, Object> expectedJsonPaths ) throws Exception {

		Files.write(inputBin, hexStringToByteArray(hexRow));
		Files.writeString(copybookFile, copybookText, StandardCharsets.US_ASCII);

		runProcess(inputBin, copybookFile, outputJson);
		// TODO: exceptions
		String json = Files.readString(outputJson, StandardCharsets.UTF_8);

		for (Map.Entry<String, Object> entry : expectedJsonPaths.entrySet()) {
			Object actual = JsonPath.read(json, entry.getKey());
			assertThat(actual, is(entry.getValue()));
		}
	}

	static List<Object[]> testData1() {
		return Arrays.asList(
			new Object[][] 
			{ 
				{
					"F1F2F3404040C1C2C3",
					String.join("\n", 
							new String[] { 
							"01 CUSTOMER-REC.", 
							"05 CUSTOMER-ID PIC X(3).",
							"05 FILLER      PIC X(3).", 
							"05 NAME        PIC X(3)." 
				}),
				Map.of("$.CUSTOMER_ID", "123", "$.NAME", "ABC") 
			}
		});
	}

	   /**
     * Generates test data for IntegrationTest.
     * Each Object[] contains:
     * 0: input hex string (binary data)
     * 1: copybook text
     * 2: expected JSON path/value map
     */
	static List<Object[]> testData() {
	    // Input hex string (from example.bin)
	    String inputHex =
	        "C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1" +
	        "C1C1C1C1C1C1C1C1C1C1C1C1C1C1F0F0" +
	        "F0F0F3F3F3F7F6C1C1F0F0F2F5F5F4F7" +
	        "F9000000000C000000000CC1F0F0F7F1" +
	        "F6F1F6F4C1";

	    // Copybook string
	    String copybook = String.join("\n", new String[] {
	        "       01 SAMPLE-REC.",
	        "          05 CUSTOMER-ID            PIC X(10).",
	        "          05 CUSTOMER-NAME          PIC X(20).",
	        "          05 ACCOUNT-NUMBER         PIC 9(9).",
	        "          05 ACCOUNT-TYPE           PIC X(2).",
	        "          05 OPEN-DATE              PIC 9(8).",
	        "          05 BALANCE                PIC S9(7)V99 COMP-3.",
	        "          05 CREDIT-LIMIT           PIC S9(7)V99 COMP-3.",
	        "          05 STATUS-CODE            PIC X(1).",
	        "          05 LAST-ACTIVITY-DATE     PIC 9(8).",
	        "          05 RESERVED-FLAG          PIC X(1)."
	    });

	    // Each element is an independent test: inputHex, copybook, JSONPath, expected value
	    return List.of(new Object[][] {
	        { inputHex, copybook, "$.SAMPLE-REC[0].CUSTOMER-ID", "AAAAAAAAAA" },
	        { inputHex, copybook, "$.SAMPLE-REC[0].CUSTOMER-NAME", "AAAAAAAAAAAAAAAAAAAA" },
	        { inputHex, copybook, "$.SAMPLE-REC[0].ACCOUNT-NUMBER", 33376 },
	        { inputHex, copybook, "$.SAMPLE-REC[0].ACCOUNT-TYPE", "AA" },
	        { inputHex, copybook, "$.SAMPLE-REC[0].OPEN-DATE", 255479 },
	        { inputHex, copybook, "$.SAMPLE-REC[0].BALANCE", 0.00 },
	        { inputHex, copybook, "$.SAMPLE-REC[0].CREDIT-LIMIT", 0.00 },
	        { inputHex, copybook, "$.SAMPLE-REC[0].STATUS-CODE", "A" },
	        { inputHex, copybook, "$.SAMPLE-REC[0].LAST-ACTIVITY-DATE", 716164 },
	        { inputHex, copybook, "$.SAMPLE-REC[0].RESERVED-FLAG", "A" }
	    });
	}

	
    static List<Object[]> testData2() {
        return List.of(
            new Object[][] {
                {
                    // Input hex string (from example.bin)
                    "C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1" +
                    "C1C1C1C1C1C1C1C1C1C1C1C1C1C1F0F0" +
                    "F0F0F3F3F3F7F6C1C1F0F0F2F5F5F4F7" +
                    "F9000000000C000000000CC1F0F0F7F1" +
                    "F6F1F6F4C1",

                    // Copybook string
                    String.join("\n", new String[] {
                        "       01 SAMPLE-REC.",
                        "          05 CUSTOMER-ID            PIC X(10).",
                        "          05 CUSTOMER-NAME          PIC X(20).",
                        "          05 ACCOUNT-NUMBER         PIC 9(9).",
                        "          05 ACCOUNT-TYPE           PIC X(2).",
                        "          05 OPEN-DATE              PIC 9(8).",
                        "          05 BALANCE                PIC S9(7)V99 COMP-3.",
                        "          05 CREDIT-LIMIT           PIC S9(7)V99 COMP-3.",
                        "          05 STATUS-CODE            PIC X(1).",
                        "          05 LAST-ACTIVITY-DATE     PIC 9(8).",
                        "          05 RESERVED-FLAG          PIC X(1)."
                    }),

                    // Expected JSON mapping
                    // NOTE: all dashes replaced with undescores
                    Map.of(
                        "$.SAMPLE-REC[0].CUSTOMER-ID", "AAAAAAAAAA",
                        "$.SAMPLE-REC[0].CUSTOMER-NAME", "AAAAAAAAAAAAAAAAAAAA",
                        "$.SAMPLE-REC[0].ACCOUNT-NUMBER", "33376",
                        "$.SAMPLE-REC[0].ACCOUNT-TYPE", "AA",
                        // "$.SAMPLE-REC[0].OPEN_DATE", "255479",
                        "$.SAMPLE-REC[0].BALANCE", 0.0,                     
                        "$.SAMPLE-REC[0].CREDIT_LIMIT", "0.00",
                        // "$.SAMPLE-REC[0].STATUS_CODE", "A",
                        "$.SAMPLE-REC[0].LAST_ACTIVITY_DATE", "716164"
                        // "$.SAMPLE-REC[0].RESERVED_FLAG", "A"
                    )
                }
            }
        );
    }

	/*
	 format-Hex -Path "Example\in\example.bin"


           Path: Example\in\example.bin

           00 01 02 03 04 05 06 07 08 09 0A 0B 0C 0D 0E 0F

00000000   C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1  ÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁÁ
00000010   C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 C1 F0 F0  ÁÁÁÁÁÁÁÁÁÁÁÁÁÁðð
00000020   F0 F0 F3 F3 F3 F7 F6 C1 C1 F0 F0 F2 F5 F5 F4 F7  ððóóó÷öÁÁððòõõô÷
00000030   F9 00 00 00 00 0C 00 00 00 00 0C C1 F0 F0 F7 F1  ù..........Áðð÷ñ
00000040   F6 F1 F6 F4 C1                                   öñöôÁ

$filePath = "C:\path\to\your\file.ext"
$bytes = Get-Content -Path $filePath -Encoding Byte -ReadCount 0
$hexString = [System.BitConverter]::ToString($bytes).Replace('-', '')
Write-Output $hexString
 
	 */
	private void runProcess(Path input, Path copybook, Path output) throws Exception {

		String javaHome = System.getenv("JAVA_HOME");
		if (javaHome == null) {
			throw new IllegalStateException("JAVA_HOME not defined");
		}

		String javaBin = Path.of(javaHome, "bin", "java").toString();
		Path jarPath = Paths.get(System.getProperty("user.dir"))
		        .resolve("..")
		        .resolve("basic-cobol2json-cb2xml-jrecord-build")
		        .resolve("build")
		        .resolve("cobol2json")
		        .resolve("target")
		        .resolve("cobolToJson-0.93.3.jar")
		        .normalize()
		        .toAbsolutePath();
		List<String> command = List.of(javaBin, 
				"-jar", 
				jarPath.toString(), 
				"-cobol", copybook.toString(), 
				"-fileOrganisation", 
				"FixedWidth",
				"-font", 
				"cp037",
				"-input",
				input.toString(), 
				"-output", 
				output.toString());

		System.err.println(String.format("Running command:\n%s",command));
		ProcessBuilder processBuilder = new ProcessBuilder( command);
		processBuilder.redirectErrorStream(true);

		// Explicit environment
		Map<String, String> env = processBuilder.environment();
		env.put("JAVA_HOME", javaHome);
		env.put("PATH", javaHome + File.separator + "bin" + File.pathSeparator + env.get("PATH"));

		Process process = processBuilder.start();

		String stdout = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);

		int exit = process.waitFor();
		if (exit != 0) {
			throw new RuntimeException("Process failed:\n" + stdout);
		}
	}

	public static byte[] hexStringToByteArray(String hexString) {
		// deal with dash or whitespace formatted hex strings
		hexString = hexString.replaceAll("[^0-9A-Fa-f]", "");
		if ((hexString.length() & 1) != 0) {
			throw new IllegalArgumentException("Odd-length hex string");
		}

		byte[] bytes = new byte[hexString.length() / 2];
		for (int i = 0; i < hexString.length(); i += 2) {
			bytes[i / 2] = (byte) Integer.parseInt(hexString.substring(i, i + 2), 16);
		}
		return bytes;
	}

}
