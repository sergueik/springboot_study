package example.utils;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.util.Random;

import example.utils.CopybookMetaParser.FieldDef;
import example.utils.CopybookMetaParser.PicType;

public class DummyValueFactory {

	private static final Random RND = new Random(42);

	public static Object valueFor(FieldDef f) {

		switch (f.type) {

		case ALPHA:
			return repeat('A', f.intDigits);

		case NUMERIC:
			return numericValue(f);

		default:
			return null;
		}
	}

	private static BigDecimal numericValue(FieldDef f) {

		long base = (long) Math.pow(10, Math.min(f.intDigits, 6)) - 1;
		long value = Math.abs(RND.nextLong()) % base;

		BigDecimal bd = BigDecimal.valueOf(value);

		if (f.fracDigits > 0) {
			bd = bd.movePointLeft(f.fracDigits);
			bd = bd.setScale(f.fracDigits, RoundingMode.UNNECESSARY);
		}

		if (f.signed && RND.nextBoolean()) {
			bd = bd.negate();
		}

		return bd;
	}

	private static String repeat(char c, int n) {
		StringBuilder sb = new StringBuilder(n);
		for (int i = 0; i < n; i++) {
			sb.append(c);
		}
		return sb.toString();
	}
}
