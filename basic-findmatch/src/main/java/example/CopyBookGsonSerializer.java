package example;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import java.lang.reflect.Type;
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

	private final Set<String> excludeFields = Set.of(
			"BRANCHID9", "AMOUNT3", "TRANDATE6", "CURSOR_POS");

	// make the code self-documenting by giving the parameter a “loud” descriptive
	// name matching its type
	@Override
	public JsonElement serialize(Map<String, Object> src, Type typeOfSrc,
			JsonSerializationContext jsonSerializationContext) {
		JsonObject jsonObject = new JsonObject();

		// Inject synthetic metadata
		jsonObject.addProperty("owner_uuid", UUID.randomUUID().toString());

		// filtering
		for (Map.Entry<String, Object> entry : src.entrySet()) {
			if (!excludeFields.contains(entry.getKey())) {
				// Gson will convert the Object value to JsonElement automatically
				jsonObject.add(entry.getKey(), jsonSerializationContext.serialize(entry.getValue()));
			}
		}

		return jsonObject;
	}
}
