package example.model;

import java.util.ArrayList;
import java.util.List;

import example.model.Node;

// generated with help of https://www.site24x7.com/tools/json-to-java.html
// NOTE: poor code generation
public class Value {
	private boolean ready;
	private String message;
	List<Node> nodes = new ArrayList<>();

	public boolean getReady() {
		return ready;
	}

	public String getMessage() {
		return message;
	}

	public void setReady(boolean data) {
		ready = data;
	}

	public void setMessage(String data) {
		message = data;
	}

	public List<Node> getNodes() {
		return nodes;
	}

	public void setNodes(List<Node> data) {
		nodes = data;
	}

}
