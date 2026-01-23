package example;
/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.math.BigDecimal;

public class DummyValueFactory {

	public static Object valueFor(CopybookMetaParser.FieldDef f) {

		String pic = f.pic.replaceAll("\\s+", "");

		if (pic.startsWith("X(")) {
			int len = Integer.parseInt(pic.replaceAll("\\D", ""));
			return DummyValueFactory.repeat("A", len);
		}

		if (pic.matches("9+")) {
			return new BigDecimal("1".repeat(pic.length()));
		}

		if (pic.matches("S9+V9+")) {
			int scale = pic.substring(pic.indexOf('V') + 1).length();
			return new BigDecimal("1050.75").setScale(scale);
		}

		if (f.usage.equals("COMP")) {
			return 123;
		}

		System.err.println("Unsupported PIC: " + f.pic + " for " + f.name);
		return null;
	}

	private static String repeat(String s, int n) {
		return s.repeat(Math.max(0, n));
	}
}
