package com.example;

import org.apache.catalina.LifecycleException;
import org.springframework.http.server.reactive.HttpHandler;
import org.springframework.http.server.reactive.ReactorHttpHandlerAdapter;
import org.springframework.web.reactive.function.server.HandlerFunction;
import org.springframework.web.reactive.function.server.RouterFunction;
import org.springframework.web.reactive.function.server.RouterFunctions;
import reactor.ipc.netty.http.server.HttpServer;

import java.io.IOException;

import static org.springframework.http.MediaType.APPLICATION_JSON;
import static org.springframework.web.reactive.function.BodyInserters.fromObject;
import static org.springframework.web.reactive.function.server.RequestPredicates.GET;
import static org.springframework.web.reactive.function.server.RouterFunctions.route;
import static org.springframework.web.reactive.function.server.ServerResponse.ok;

class Hello {
	private final String name;

	// removal of default constructor leads to
	// com.fasterxml.jackson.databind.exc.MismatchedInputException:
	// Cannot construct instance of `com.example.Hello`
	// (although at least one Creator exists):
	// cannot de-serialize from Object value
	// (no delegate- or property-based Creator)
	public Hello() {
		this.name = null;
	}

	public Hello(String _name) {
		this.name = _name;
	}

	public String getName() {
		return name;
	}

	// added silently by lombok
	protected boolean canEqual(Object other) {
		return other instanceof Hello;
	}

	@Override
	public boolean equals(Object o) {
		if (o == this)
			return true;
		if (!(o instanceof Hello))
			return false;
		Hello other = (Hello) o;
		if (!other.canEqual((Object) this))
			return false;
		if (this.getName() == null ? other.getName() != null
				: !this.getName().equals(other.getName()))
			return false;
		return true;
	}

	@Override
	public int hashCode() {
		int result = 1;
		result = this.getName().hashCode();
		return result;
	}

	@Override
	public String toString() {
		return "Hello(name = \"" + this.getName() + "\" )";
	}
}

public class FunctionalWebApplication {

	@SuppressWarnings({ "unchecked", "rawtypes" })
	static RouterFunction getRouter() {
		HandlerFunction hello = request -> ok().body(fromObject("Hello"));
		return route(GET("/"), hello).andRoute(GET("/json"), req -> ok()
				.contentType(APPLICATION_JSON).body(fromObject(new Hello("world"))));
	}

	public static void main(String[] args)
			throws IOException, LifecycleException, InterruptedException {
		HttpHandler httpHandler = RouterFunctions.toHttpHandler(getRouter());
		HttpServer.create("localhost", 8080)
				.newHandler(new ReactorHttpHandlerAdapter(httpHandler)).block();
		Thread.currentThread().join();
	}
}
