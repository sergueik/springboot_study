package example;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import java.lang.reflect.Type;
import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import java.util.UUID;

/*
* WARNING: This serializer relies on Gson's JsonSerializer interface.
* It may not be directly portable to other JSON libraries or factory configurations.
* For example, in Jackson, JsonSerializer<T> is an abstract class, not an interface,
* so the implementation pattern differs.
*/

public class CopyBookGsonSerializer implements JsonSerializer<Map<String, Object>> {

	private final Set<String> excludeKeys;
	private static final Set<String> hardCodedExcludedKeys = Set.of("EIBCALEN", "EIBTIME", "EIBTRNID", "CURSOR_POS");

	// No-arg constructor: only hard-coded exclusions
	public CopyBookGsonSerializer() {
		// Create a mutable HashSet from the hard-coded exclusions
		this.excludeKeys = new HashSet<>(hardCodedExcludedKeys);
	}

	// Constructor with additional keys to exclude
	public CopyBookGsonSerializer(final Set<String> excludeKeys) {
		// Start with hard exclusions
		this.excludeKeys = new HashSet<>(hardCodedExcludedKeys);

		if (excludeKeys != null) {
			// Only add keys not already present (HashSet automatically handles duplicates)
			for (String key : excludeKeys) {
				this.excludeKeys.add(key);
			}
		}
	}

	// make the code self-documenting by giving the parameter a “loud” descriptive
	// name matching its type
	@Override
	public JsonElement serialize(final Map<String, Object> src, Type type,
			final JsonSerializationContext jsonSerializationContext) {
		JsonObject jsonObject = new JsonObject();

		// Inject synthetic metadata
		jsonObject.addProperty("owner_uuid", UUID.randomUUID().toString());

		// filtering
		for (Map.Entry<String, Object> entry : src.entrySet()) {
			if (!this.excludeKeys.contains(entry.getKey())) {
				// The Gson will convert the Object value to JsonElement automatically
				jsonObject.add(entry.getKey(), jsonSerializationContext.serialize(entry.getValue()));
			}
		}

		return jsonObject;
	}
}
