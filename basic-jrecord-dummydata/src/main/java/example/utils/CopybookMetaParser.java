package example.utils;

/**
 * Copyright 2026 Serguei Kouzmine
 */
import java.io.*;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.*;
import java.util.regex.*;

@SuppressWarnings("unused")
public class CopybookMetaParser {

	public static class FieldDef {
		public final String name;
		public final PicType type;
		public final int intDigits;
		public final int fracDigits;
		public final boolean signed;
		public final boolean comp3;
		public final int level;

		public FieldDef(String name, PicType type, int intDigits, int fracDigits, boolean signed, boolean comp3,
				int level) {
			this.name = name;
			this.type = type;
			this.intDigits = intDigits;
			this.fracDigits = fracDigits;
			this.signed = signed;
			this.comp3 = comp3;
			this.level = level;
		}
	}

	public enum PicType {
		ALPHA, NUMERIC
	}

	/*
	 * Handles: PIC X(10) PIC 9(9) PIC S9(7)V99 PIC S9(7)V9(2) COMP-3
	 */
	private static final Pattern PIC_PATTERN = Pattern.compile("PIC\\s+" + "(S?)" + // signed
			"(?:X\\((\\d+)\\)|" + // alpha
			"9\\((\\d+)\\)" + // numeric integer digits
			"(?:V9*\\(?([0-9]+)\\)?)?)" + // decimal digits
			"\\s*(COMP-3)?", Pattern.CASE_INSENSITIVE);

	private static final Pattern LINE_PATTERN = Pattern.compile("\\s*(\\d{2})\\s+([A-Z0-9-]+)\\s*(.*)",
			Pattern.CASE_INSENSITIVE);

	public static List<FieldDef> parse(Path copybookFile) throws IOException {
		List<FieldDef> fields = new ArrayList<>();
		Deque<Integer> levelStack = new ArrayDeque<>();

		for (String line : Files.readAllLines(copybookFile)) {
			line = line.trim();
			if (line.isEmpty() || line.startsWith("*")) {
				continue;
			}

			Matcher lm = LINE_PATTERN.matcher(line);
			if (!lm.matches()) {
				continue;
			}

			int level = Integer.parseInt(lm.group(1));
			String name = lm.group(2);
			String rest = lm.group(3);

			while (!levelStack.isEmpty() && levelStack.peek() >= level) {
				levelStack.pop();
			}

			Matcher pm = PIC_PATTERN.matcher(rest);
			if (!pm.find()) {
				// group or unsupported field
				levelStack.push(level);
				System.err.println("WARN: skipping group or unsupported field: " + name);
				continue;
			}

			boolean signed = !pm.group(1).isEmpty();
			String xLen = pm.group(2);
			String intDigitsStr = pm.group(3);
			String fracDigitsStr = pm.group(4);
			boolean comp3 = pm.group(5) != null;

			if (xLen != null) {
				fields.add(new FieldDef(name, PicType.ALPHA, Integer.parseInt(xLen), 0, false, false, level));
			} else {
				int intDigits = Integer.parseInt(intDigitsStr);
				int fracDigits = fracDigitsStr != null ? Integer.parseInt(fracDigitsStr) : 0;

				fields.add(new FieldDef(name, PicType.NUMERIC, intDigits, fracDigits, signed, comp3, level));
			}
		}

		return fields;
	}
}
