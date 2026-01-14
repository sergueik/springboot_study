package example;

import java.util.regex.Pattern;

public class RegexFromCopyBook {

	private RegexFromCopyBook() {
	}

	public static Pattern compile(CopyBookSpec copyBookSpec) {
		String regex = buildRegex(copyBookSpec);
		return Pattern.compile(regex);
	}

	// Build regex and also allow access as string
	public static String buildRegex(CopyBookSpec copyBookSpec) {
		StringBuilder sb = new StringBuilder(1024);
		sb.append("^");

		for (FieldSpec field : copyBookSpec.fields) {
			sb.append(buildGroup(field));
		}

		sb.append("$");
		return sb.toString();
	}

	private static String buildGroup(final FieldSpec fieldSpec) {
		String pattern;

		switch (fieldSpec.type.toLowerCase()) {
		case "string":
			pattern = ".{" + fieldSpec.length + "}";
			break;

		case "number":
			pattern = digitGroup(fieldSpec.length);
			break;

		case "date":
			pattern = digitGroup(fieldSpec.length);
			break;

		default:
			throw new IllegalArgumentException("Unsupported field type: " + fieldSpec.type);
		}

		return "(?<" + sanitize(fieldSpec.name) + ">" + pattern + ")";
	}

	private static String sanitize(String name) {
		return name.replaceAll("[^A-Za-z0-9]", "");
	}

	private static String digitGroup(int length) {
		return "\\d{" + length + "}";
	}
}
