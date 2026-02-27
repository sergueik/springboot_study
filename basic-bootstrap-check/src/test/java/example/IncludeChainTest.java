package example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.junit.Test;

import com.google.gson.Gson;

public class IncludeChainTest {

	private Map<String, List<String>> files = new HashMap<>();
	private Map<String, Set<String>> datafiles = new HashMap<>();;

	private void addData(String name) {
		addData(name, null);
	}

	private void addData(String name, String caller) {
		if (caller == null) {
			caller = name;
		}
		System.err.println("name: " + name);
		Set<String> data = new HashSet<>();

		if (datafiles.containsKey(caller)) {
			data = datafiles.get(caller);
		}

		List<String> contents = files.get(name);
		for (String line : contents) {
			System.err.println("processing line: " + line);
			if (line.matches("#include.*$")) {
				String newname = line.split(" ")[1];
				System.err.println("call with : name=" + newname + " caller= " + name);
				addData(newname, name);
				addData(newname, caller);

			} else {
				System.err.println("adding: " + line + " to " + caller);
				data.add(line);
			}
		}
		datafiles.put(caller, data);
	}

	@Test
	public void test() throws Exception {

		files.put("a", Arrays.asList("#include b", "#include c", "#include e",
				"data a1", "data a2"));
		files.put("b", Arrays.asList("#include c", "#include e", "data b1",
				"data b2", "data b3"));
		files.put("c", Arrays.asList("#include d", "data c1", "data c2"));
		files.put("d", Arrays.asList("data d1", "data d2"));
		files.put("e", Arrays.asList("data e1", "data e2", "data e3"));

		addData("a");

		for (String name : files.keySet()) {
			System.err.println("first scan: " + name);
			addData(name);
		}

		for (String name : datafiles.keySet()) {
			System.err.println(name + " " + datafiles.get(name) + "\n");
		}
	}

}
