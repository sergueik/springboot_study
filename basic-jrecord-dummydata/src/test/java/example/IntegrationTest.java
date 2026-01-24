package example;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.CoreMatchers.is;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.file.*;
import java.nio.charset.StandardCharsets;
import java.util.*;
import java.io.*;

import org.junit.jupiter.api.BeforeAll;
// import org.junit.jupiter.api.Disabled;
import org.junit.jupiter.api.DisplayName;
// import org.junit.jupiter.api.Test;

import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.MethodSource;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.jayway.jsonpath.JsonPath;

public class IntegrationTest {

	private static Path tempDir = null;
	private static Path input = null;
	private static Path copybook = null;
	private static Path output = null;
	private static String javaHome = null;

	@BeforeAll
	public static void setup() throws IOException {
		javaHome = System.getenv("JAVA_HOME");
		if (javaHome == null) {
			throw new IllegalStateException("JAVA_HOME not defined");
		}
		tempDir = Files.createTempDirectory("coboltest");
		input = tempDir.resolve("input.bin");
		copybook = tempDir.resolve("copybook.cpy");
		output = tempDir.resolve("output.json");
	}

	@DisplayName("Parse a binary file and a copybook, examine the fields")
	@ParameterizedTest
	@MethodSource("testData")
	public void test1(String hexRow, String copybookText, String jsonPath, Object value) throws Exception {

		Files.write(input, hexStringToByteArray(hexRow));
		Files.writeString(copybook, copybookText, StandardCharsets.US_ASCII);
		runProcess(input, copybook, output);
		// TODO: assert no exceptions come from runProcess		
		String json = Files.readString(output, StandardCharsets.UTF_8);

		Object actual = JsonPath.read(json, jsonPath);
		assertThat(actual, is(value));
	    System.err.println(String.format("Verified value:  %s = %s", jsonPath, value));
	}

	// @Disabled
	@DisplayName("Parse a binary file and a copybook, expect failure on invalid data")
	@ParameterizedTest
	@MethodSource("testData2")
	public void test2(String hexRow, String copybookText,  Map<String, Object> expectedData ) throws Exception {

		Files.write(input, hexStringToByteArray(hexRow));
		Files.writeString(copybook, copybookText, StandardCharsets.US_ASCII);
		runProcess(input, copybook, output);
		String json = Files.readString(output, StandardCharsets.UTF_8);

		List<Map.Entry<String, Object>> entries = new ArrayList<>(expectedData.entrySet());
	    Collections.shuffle(entries);
	    Exception e = assertThrows(Exception.class, () -> {
	        for (Map.Entry<String, Object> entry : entries) {
	            Object actual = JsonPath.read(json, entry.getKey());
	            assertThat(actual, is(entry.getValue()));
	        }
	    });
	    /*
	    */

	    System.err.println("Expected exception caught: " + e.getMessage());
	}

  	static List<Object[]> testData() {
	   // Input hex and copybook (same for all paths)
	    String inputHex = 
	        "C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1" +
	        "C1C1C1C1C1C1C1C1C1C1C1C1C1C1F0F0" +
	        "F0F0F3F3F3F7F6C1C1F0F0F2F5F5F4F7" +
	        "F9000000000C000000000CC1F0F0F7F1" +
	        "F6F1F6F4C1";

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

	    // Map of JSON paths to expected values
	    Map<String, Object> expectedPaths = Map.of(
	        "$.SAMPLE-REC[0].CUSTOMER-ID", "AAAAAAAAAA",
	        "$.SAMPLE-REC[0].CUSTOMER-NAME", "AAAAAAAAAAAAAAAAAAAA",
	        "$.SAMPLE-REC[0].ACCOUNT-NUMBER", 33376,
	        "$.SAMPLE-REC[0].ACCOUNT-TYPE", "AA",
	        "$.SAMPLE-REC[0].OPEN-DATE", 255479,
	        "$.SAMPLE-REC[0].BALANCE", 0.0,
	        "$.SAMPLE-REC[0].CREDIT-LIMIT", 0.0,
	        "$.SAMPLE-REC[0].STATUS-CODE", "A",
	        "$.SAMPLE-REC[0].LAST-ACTIVITY-DATE", 716164,
	        "$.SAMPLE-REC[0].RESERVED-FLAG", "A"
	    );

	    // Convert to List<Object[]> for ParameterizedTest
	    return expectedPaths.entrySet()
	        .stream()
	        .map((Map.Entry<String, Object> entry) -> new Object[] { inputHex, copybook, entry.getKey(), entry.getValue() })
	        .collect(Collectors.toList());
	}

	// same test data but with multiple json paths evaluated in a single test method call
    static List<Object[]> testData2() {
        return List.of(
            new Object[][] {
                {
                    "C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1C1" +
                    "C1C1C1C1C1C1C1C1C1C1C1C1C1C1F0F0" +
                    "F0F0F3F3F3F7F6C1C1F0F0F2F5F5F4F7" +
                    "F9000000000C000000000CC1F0F0F7F1" +
                    "F6F1F6F4C1",

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

                    Map.of(
                        "$.SAMPLE-REC[0].CUSTOMER-ID", "AAAAAAAAAA",
                        "$.SAMPLE-REC[0].CUSTOMER-NAME", "AAAAAAAAAAAAAAAAAAAA",
                        "$.SAMPLE-REC[0].ACCOUNT-NUMBER", "33376",
                        "$.SAMPLE-REC[0].ACCOUNT-TYPE", "AA",
                        "$.SAMPLE-REC[0].OPEN_DATE", "255479",
                        "$.SAMPLE-REC[0].BALANCE", 0.0,                     
                        "$.SAMPLE-REC[0].CREDIT_LIMIT", "0.00",
                        "$.SAMPLE-REC[0].STATUS_CODE", "A",
                        "$.SAMPLE-REC[0].LAST_ACTIVITY_DATE", "716164",
                        "$.SAMPLE-REC[0].RESERVED_FLAG", "A"
                    )
                }
            }
        );
    }

	private void runProcess(Path input, Path copybook, Path output) throws Exception {


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
