package example;

import java.util.HashMap;
import java.util.Iterator;

// https://www.digitalocean.com/community/tutorials/how-to-create-immutable-class-in-java

public final class ExampleClass {
	private final int id;
	private final String name;
	private final HashMap<String, String> hashMap;

	public int getId() {
		return id;
	}

	public String getName() {
		return name;
	}

	// Getter function for mutable objects

	@SuppressWarnings("unchecked")
	public HashMap<String, String> getHashMap() {
		return (HashMap<String, String>) hashMap.clone();
	}

	// NOTE: Constructor method performs deep copy of the constructor input
	// during initialization

	public ExampleClass(int id, String name, HashMap<String, String> hashMap) {
		this.id = id;
		this.name = name;

		HashMap<String, String> tempMap = new HashMap<String, String>();
		Iterator<String> it = hashMap.keySet().iterator();
		while (it.hasNext()) {
			String key = it.next();
			tempMap.put(key, hashMap.get(key));
		}
		this.hashMap = tempMap;
	}

}
