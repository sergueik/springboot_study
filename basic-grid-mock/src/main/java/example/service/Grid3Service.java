package example.service;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.stereotype.Service;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.model.grid3.Build;
import example.model.grid3.Data;
import example.model.grid3.Value;
import example.utils.Utils;

@Service
public class Grid3Service {

	private boolean debug = false;
	private final String version = "3.9.2";
	private static final Gson gson = new GsonBuilder().setPrettyPrinting()
			.create();

	public Data status() {
		final String result = Utils.getScriptContent("grid3.json");
		Data data = gson.fromJson(result, Data.class);
		Value value = data.getValue();
		Build build = value.getBuild();

		build.setVersion(version);
		value.setBuild(build);
		data.setValue(value);
		return data;
	}

}
