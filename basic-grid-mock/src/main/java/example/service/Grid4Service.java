package example.service;
/**
 * Copyright 2022 Serguei Kouzmine
 */

import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import com.google.gson.Gson;
import com.google.gson.GsonBuilder;

import example.model.grid4.Data;
import example.model.grid4.Node;
import example.model.grid4.Value;
import example.utils.Utils;

@Service
public class Grid4Service {

	private static final Gson gson = new GsonBuilder().setPrettyPrinting()
			.create();

	public Data status() {
		final String result = Utils.getScriptContent("grid4.json");
		Data data = gson.fromJson(result, Data.class);
		Node node1 = new Node();
		node1.setAvailability("UP");
		node1.setId("node1 added manually");
		Node node2 = new Node();
		node2.setAvailability("DOWN");
		node2.setId("node2 added manually");
		// for GUID constructor
		// see also:
		// http://www.java2s.com/Code/Java/Development-Class/RandomGUID.htm
		Value value = data.getValue();
		List<Node> nodes = value.getNodes();

		// cannot use Stream with counter increment:
		// int cnt = 0;
		// nodes.stream()
		// .forEach(o -> o.setUri(String.format("http://node%02d:5555", cnt++)));
		// local variable cnt defined in an enclosing scope must be final or
		// effectively final
		for (int cnt = 0; cnt != nodes.size(); cnt++) {
			nodes.get(cnt).setUri(String.format("http://node%02d:5555", cnt));
		}
		nodes.add(node1);
		nodes.add(node2);
		value.setNodes(nodes);
		data.setValue(value);
		return data;
	}
}
