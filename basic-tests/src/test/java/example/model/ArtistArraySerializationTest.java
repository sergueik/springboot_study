package example.model;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
// Compilation failure
// reference to containsString is ambiguous
// import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.Matchers.is;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import com.google.gson.Gson;
import com.google.gson.internal.LinkedTreeMap;

@SuppressWarnings("unchecked")

public class ArtistArraySerializationTest {
	private static List<Artist> artists = new ArrayList<>();
	private static String payload1 = null;
	private static String payload2 = null;
	private static String payload3 = null;
	private Map<String, Object> response = new HashMap<>();
	private static final Gson gson = new Gson();

	@BeforeEach
	public void setUp() {
		artists.clear();
		for (int cnt = 0; cnt != 4; cnt++) {
			Artist artist = new Artist(cnt, "", "");
			artist.setName("name " + cnt);
			artist.setPlays("unknown");
			artists.add(artist);
		}
		response.clear();
		response.put("status", true);
		response.put("result", artists);
		payload1 = gson.toJson(artists);
		payload2 = gson.toJson(response);
	}

	@Test
	public void test2() throws Exception {
		System.err.println("payload1: " + payload1);
		ArrayList<Object> results1 = (ArrayList<Object>) gson.fromJson(payload1,
				ArrayList.class);
		assertThat(results1, notNullValue());
		assertThat(results1.size(), is(4));
		for (int cnt = 0; cnt != results1.size(); cnt++) {
			String chunk = gson.toJson(results1.get(cnt));
			Artist result = gson.fromJson(chunk, Artist.class);
			assertThat(result, notNullValue());
			assertThat(result.getId(), is(cnt));
			assertThat(result.getPlays(), is("unknown"));
		}
	}

	@Test
	public void test3() throws Exception {
		System.err.println("payload1: " + payload1);
		ArrayList<LinkedTreeMap<String, Object>> results2 = gson.fromJson(payload1,
				ArrayList.class);
		assertThat(results2, notNullValue());
		assertThat(results2.size(), is(4));
		for (int cnt = 0; cnt != results2.size(); cnt++) {
			String chunk = gson.toJson(results2.get(cnt));
			Artist result = gson.fromJson(chunk, Artist.class);
			assertThat(result, notNullValue());
			assertThat(result.getId(), is(cnt));
			assertThat(result.getPlays(), is("unknown"));
		}
	}

	@Test
	public void test4() throws Exception {
		System.err.println("payload2: " + payload2);

		Map<String, Object> result3 = gson.fromJson(payload2, Map.class);
		String payload3 = gson.toJson(result3.get("result"));
		ArrayList<LinkedTreeMap<String, Object>> results2 = gson.fromJson(payload3,
				ArrayList.class);
		assertThat(results2, notNullValue());
		assertThat(results2.size(), is(4));
		for (int cnt = 0; cnt != results2.size(); cnt++) {
			String chunk = gson.toJson(results2.get(cnt));
			System.err.println("chunk " + cnt + ": " + chunk);
			Artist result = gson.fromJson(chunk, Artist.class);
			assertThat(result, notNullValue());
			assertThat(result.getId(), is(cnt));
			assertThat(result.getPlays(), is("unknown"));
		}
	}

}
