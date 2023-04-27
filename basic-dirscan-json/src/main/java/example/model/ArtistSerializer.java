package example.model;
/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.lang.reflect.Type;
import com.google.gson.FieldNamingStrategy;
import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

import org.json.JSONException;
import org.json.JSONObject;
import org.json.XML;

import example.model.Artist;

// https://stackoverflow.com/questions/11038553/serialize-java-object-with-gson
public class ArtistSerializer implements JsonSerializer<Artist> {
	@Override
	public JsonElement serialize(final Artist data, final Type type,
			final JsonSerializationContext context) {
		JsonObject result = new JsonObject();
		String name = data.getName();
		result.add("name", new JsonPrimitive(name));

		String plays = data.getPlays();
		if (plays != null && !plays.isEmpty()) {
			result.add("plays", new JsonPrimitive(plays));
		}

		@SuppressWarnings("unused")
		// filter what to (not) serialize
		Float price = data.getPrice();
		// result.add("price", new JsonPrimitive(price));

		int id = data.getId();
		if (id != 0) {
			result.add("id", new JsonPrimitive(id));
		}
		return result;
	}

}
