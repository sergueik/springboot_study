package example;

/**
 * Copyright 2014,2021,2026 Serguei Kouzmine
 */
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class FindMatch {

	private Pattern pattern;
	private Matcher matcher;
	private Map<String, String> results;

	private final String tagMatcher = "(?:<(?<result>[^>]+)>)";

	public Map<String, String> findMatch(String data, String patternExression) {
		return findMatch(data, patternExression, resolveGroups(patternExression));
	}

	// NOTE: capturing just one of each groups
	public Map<String, String> findMatch(String data, String patternExression, List<String> groups) {
		Map<String, String> matches = new HashMap<>();
		pattern = Pattern.compile(patternExression);
		matcher = pattern.matcher(data);
		if (matcher.find()) {
			System.err.println("findMatch data:" + data);
			for (String name : groups) {
				String value = matcher.group(name).toString();
				matches.put(name, value);
			}
		}
		return matches;
	}

	public List<String> resolveGroups(String patternExression) {
		List<String> groups = new ArrayList<>();
		if (null == patternExression) {
			return null;
		}
		Pattern p = Pattern.compile(tagMatcher);
		Matcher m = p.matcher(patternExression);
		while (m.find()) {
			String name = m.group("result");
			groups.add(name);
		}
		System.err.println("data:" + groups.toString());
		return groups;
	}

}
