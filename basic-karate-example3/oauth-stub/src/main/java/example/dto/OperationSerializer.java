package example.dto;

import java.lang.reflect.Type;

import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

public class OperationSerializer implements JsonSerializer<Operation> {

	@Override
	public JsonElement serialize(final Operation data, final Type type, final JsonSerializationContext context) {
		JsonObject result = new JsonObject();
		return result;
	}

}
