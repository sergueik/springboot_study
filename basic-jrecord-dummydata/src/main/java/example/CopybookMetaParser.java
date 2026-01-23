package example;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.io.*;
import java.math.BigDecimal;
import java.nio.file.*;
import java.util.*;
import java.util.regex.*;

@SuppressWarnings("unused")
public class CopybookMetaParser {

	public static class FieldDef {
		public final String name;
		public final String pic;
		public final String usage;

		FieldDef(String name, String pic, String usage) {
			this.name = name;
			this.pic = pic;
			this.usage = usage;
		}
	}

	private static final Pattern FIELD_PATTERN = Pattern.compile(
			"^\\s*(\\d{2})\\s+([A-Z0-9-]+)\\s+PIC\\s+([^\\.]+)(?:\\s+(COMP-3|COMP))?\\.", Pattern.CASE_INSENSITIVE);

	public static List<FieldDef> parse(Path copybookFile) throws IOException {
		List<FieldDef> fields = new ArrayList<>();

		for (String line : Files.readAllLines(copybookFile)) {
			line = line.trim();
			if (line.isEmpty() || line.startsWith("*"))
				continue;

			Matcher m = FIELD_PATTERN.matcher(line);
			if (!m.find()) {
				if (line.matches("^\\s*\\d{2}.*\\.$")) {
					System.err.println("Ignored unsupported line: " + line);
				}
				continue;
			}

			int level = Integer.parseInt(m.group(1));
			if (level != 5) {
				System.err.println("Ignored non-05 level: " + line);
				continue;
			}

			fields.add(new FieldDef(m.group(2), m.group(3).toUpperCase(),
					m.group(4) == null ? "DISPLAY" : m.group(4).toUpperCase()));
		}
		return fields;
	}
}
