package example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

@SuppressWarnings("serial")
public class CopyBook extends HashMap<String, String> {
	public CopyBook() {
		super();
	}

	public CopyBook(Map<String, String> values) {
		super(values);
	}
}
