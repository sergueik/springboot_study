package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertFalse;
import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.greaterThan;
import static org.hamcrest.Matchers.hasKey;

import java.util.Set;
import java.util.UUID;
import java.util.HashSet;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.io.StringWriter;
import java.lang.reflect.Type;
import java.util.Arrays;

import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Test;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;
import com.google.gson.JsonElement;
import com.google.gson.JsonObject;
import com.google.gson.JsonPrimitive;
import com.google.gson.JsonSerializationContext;
import com.google.gson.JsonSerializer;

//origin: https://www.baeldung.com/java-serialization

public class JsonSerializerSerializationTest {
	private boolean debug = true;

	// https://stackoverflow.com/questions/11038553/serialize-java-object-with-gson
	public static class ArtistSerializer implements JsonSerializer<Artist> {
		@Override
		public JsonElement serialize(final Artist data, final Type type, final JsonSerializationContext context) {
			JsonObject result = new JsonObject();
			int id = data.getId();
			if (id != 0) {
				result.add("id", new JsonPrimitive(id));
			}
			// NOTE: cannot serialize static info from the serialized class
			// NPE
			/*
			 * if (type != null) { result.add("staticInfo", new
			 * JsonPrimitive(((Artist) type).getStaticInfo())); } else { String
			 * staticInfo = data.getStaticInfo(); System.err.println(
			 * "Static info: " + staticInfo); if (staticInfo != null) {
			 * result.add("staticInfo", new JsonPrimitive(staticInfo)); } }
			 */
			@SuppressWarnings("unused")
			String name = data.getName();
			result.add("name", new JsonPrimitive(name));

			String plays = data.getPlays();
			if (plays != null && !plays.isEmpty()) {
				result.add("plays", new JsonPrimitive(plays));
			}
			/*
			 * Float price = data.getPrice(); result.add("price", new
			 * JsonPrimitive(price));
			 */
			return result;
		}

	}

	// https://stackoverflow.com/questions/11038553/serialize-java-object-with-gson
	public static class ArtistSerializerNoName implements JsonSerializer<Artist> {
		@Override
		public JsonElement serialize(final Artist data, final Type type, final JsonSerializationContext context) {
			JsonObject result = new JsonObject();
			int id = data.getId();
			if (id != 0) {
				result.add("id", new JsonPrimitive(id));
			}
			@SuppressWarnings("unused")
			String name = data.getName();
			// NOTE:
			// can filter what to (not) serialize

			String plays = data.getPlays();
			if (plays != null && !plays.isEmpty()) {
				result.add("plays", new JsonPrimitive(plays));
			}
			/*
			 * Float price = data.getPrice(); result.add("price", new
			 * JsonPrimitive(price));
			 */
			return result;
		}

	}

	public static class Artist implements Serializable {

		private String name;
		private String plays;
		private static String staticInfo;

		private transient int id;

		public String getName() {
			return name;
		}

		public void setName(String data) {
			this.name = data;
		}

		public String getPlays() {
			return plays;
		}

		public void setPlays(String data) {
			this.plays = data;
		}

		public int getId() {
			return id;
		}

		public void setId(int data) {
			this.id = data;
		}

		public Artist() {
			staticInfo = UUID.randomUUID().toString();
		}

		public /* static */ String getStaticInfo() {
			return Artist.staticInfo;
		}

		public Artist(int id, String name, String plays) {
			super();
			if (Artist.staticInfo == null) {
				Artist.staticInfo = UUID.randomUUID().toString();
			}
			this.name = name;
			this.id = id;
			this.plays = plays;
		}

	}

	private JsonSerializer<Artist> artistSerializer = null;
	private Gson gson = new Gson();
	private Artist artist;

	@Before
	public void before() {
		artist = new Artist(1, "john", "guitar");
	}

	@Test
	public void test1() throws IOException, ClassNotFoundException {
		artistSerializer = new ArtistSerializer();
		gson = new GsonBuilder().registerTypeAdapter(Artist.class, artistSerializer).create();
		String json = gson.toJson(artist);
		System.err.println("test1: " + json);
		Artist a2 = gson.fromJson(json, Artist.class);

	}

	@Test
	public void test2() throws IOException, ClassNotFoundException {
		artistSerializer = new ArtistSerializerNoName();
		gson = new GsonBuilder().registerTypeAdapter(Artist.class, artistSerializer).create();
		String json = gson.toJson(artist);
		System.err.println("test2: " + json);
		Artist a2 = gson.fromJson(json, Artist.class);

	}

	@Test
	// http://www.java2s.com/example/java-api/java/io/stringwriter/stringwriter-0-17.html
	public void test3() {

		StringWriter stringWriter = new StringWriter();
		gson = new GsonBuilder().setPrettyPrinting().create();
		gson.toJson(artist, stringWriter);
		StringBuffer stringBuffer = stringWriter.getBuffer();
		String json = stringBuffer.toString();
		System.err.println("test3: " + json);
		Artist a2 = gson.fromJson(json, Artist.class);
		assertThat(a2.getId(), is(0));
		assertThat(a2.getName(), is(artist.getName()));
	}

}
