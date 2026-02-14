package example.graphviz;

import static org.hamcrest.MatcherAssert.assertThat;
import static org.hamcrest.Matchers.is;
import static org.hamcrest.Matchers.notNullValue;

import java.awt.image.BufferedImage;
import java.io.ByteArrayInputStream;
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.util.stream.Stream;

import javax.imageio.ImageIO;

import org.junit.jupiter.api.AfterAll;
import org.junit.jupiter.api.BeforeAll;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.Arguments;
import org.junit.jupiter.params.provider.MethodSource;

import example.DotGraphics;
import example.GraphViz;

public class UmlGalleryParameterizedTest {

	static DotGraphics server;
	private static int port;
	private HttpRequest request = null;
	private HttpResponse<byte[]> response = null;
	private byte[] body = {};
	private BufferedImage img = null;
	private static String url = null;

	@BeforeAll
	static void startServer() throws Exception {
		port = getFreePort();
		// port = 18080; // or pick a fixed port
		server = new DotGraphics(port);
		url = "http://localhost:" + port + "/";
		server.start();
	}

	@AfterAll
	static void stopServer() {
		// Gracefully stop the DotGraphics server
		// Closing the ServerSocket forcibly unblocks the listener thread,
		// which will throw a SocketException ("Socket closed") in the thread.
		// This is expected and harmless: it only indicates the listener
		// noticed the shutdown. All requests have already completed.
		server.stop();
	}

	static Stream<Arguments> graphvizGalleryPayloads() {
		return Stream.of(

				// UML Class Diagram examples (directed)
				// Gallery: UML Class diagrams
				Arguments.of("UML1 - Simple Class", """
						digraph UML1 {
						  node [shape=record];
						  ClassA [label="{ClassA|+field1:int|+method1()}"];
						  ClassB [label="{ClassB|+field2:String|+method2()}"];
						  ClassA -> ClassB [label="uses"];
						}
						"""),

				Arguments.of("UML2 - Inheritance", """
						digraph UML2 {
						  node [shape=record];
						  Animal [label="{Animal|+name:String|+eat()}"];
						  Dog [label="{Dog||+bark()}"];
						  Dog -> Animal [arrowhead="empty", label="extends"];
						}
						"""),
				// Source (approx):
				// https://graphviz.org/Gallery/directed/UML_Class_diagram.html
				Arguments.of("Composition Example", """
						digraph UML3 {
						  node [shape=record];
						  Car [label="{Car|engine:Engine|drive()}"];
						  Engine [label="{Engine|power:int|start()}"];
						  Car -> Engine [label="has-a"];
						}
						"""),

				// Source (approx):
				// https://graphviz.org/Gallery/directed/UML_Class_diagram.html
				Arguments.of("Interface-like Example", """
						digraph UML4 {
						  node [shape=record];
						  IService [label="{<<interface>> IService|+execute()}"];
						  ServiceImpl [label="{ServiceImpl||+execute()}"];
						  ServiceImpl -> IService [arrowhead="empty", style=dashed];
						}
						"""),

				// Neato ER example (undirected with neato layout)
				// Gallery: Entity-Relation Data Model (neato)
				Arguments.of("Neato ER - Entity Relation", """
						graph ER {
						  layout=neato;
						  node [shape=box]; course; institute; student;
						  node [shape=ellipse]; name0; name1; name2;
						  node [shape=diamond,style=filled,color=lightgrey]; "C-I"; "S-C"; "S-I";
						  name0 -- course;
						  course -- "C-I" [label="n",len=1.00];
						  "C-I" -- institute [label="1",len=1.00];
						  institute -- "S-I" [label="1",len=1.00];
						  "S-I" -- student [label="n",len=1.00];
						  student -- name2;
						}
						"""),

				// Neato dependency graph (simplified)
				// Gallery: softmaint (neato)
				Arguments.of("Neato Dependency - Simplified", """
						digraph SoftMaintSimple {
						  layout=neato;
						  A -> B;
						  A -> C;
						  C -> D;
						  B -> D;
						}
						"""),

				// Parsing state subset (lean version of psg)
				// Gallery: Parsing Tree (psg)
				Arguments.of("Parsing Tree - Lean", """
						digraph ParseLean {
						  rankdir=LR;
						  node [shape=record];
						  S0 [label="{State0|s->·A}"];
						  S1 [label="{State1|A->·a}"];
						  S0 -> S1 [label="shift a"];
						}
						"""),

				// FSM example (simple)
				// Gallery: Finite State Machine (fsm)
				Arguments.of("FSM - Simple Machine", """
						digraph fsmSimple {
						  rankdir=LR;
						  node [shape = circle];
						  start [shape=plaintext];
						  start -> Idle;
						  Idle -> Running [label="go"];
						  Running -> Idle [label="stop"];
						}
						"""),

				// Minimal FSM sample
				Arguments.of("FSM - Tiny Loop", """
						digraph tinyFSM {
						  rankdir=LR;
						  node [shape=circle];
						  A -> B [label="toB"];
						  B -> A [label="toA"];
						}
						"""));
	}

	@ParameterizedTest(name = "{0}")
	@MethodSource("graphvizGalleryPayloads")
	void renderUmlGallerySamples(String name, String dotPayload) throws Exception {

		request = HttpRequest.newBuilder().uri(URI.create(url)).POST(HttpRequest.BodyPublishers.ofString(dotPayload))
				.build();

		response = HttpClient.newHttpClient().send(request, HttpResponse.BodyHandlers.ofByteArray());

		assertThat(response.statusCode(), is(200));

		body = response.body();
		assertThat(body, notNullValue());

		try (ByteArrayInputStream in = new ByteArrayInputStream(body)) {
			img = ImageIO.read(in);
			assertThat(img, notNullValue());
			assertThat(img.getType(), is(BufferedImage.TYPE_4BYTE_ABGR));

			System.out.printf("Rendered [%s] -> PNG %d x %d%n", name, img.getWidth(), img.getHeight());
		}
	}

	// Utility method assumed to exist in your project
	private static int getFreePort() throws Exception {
		try (java.net.ServerSocket socket = new java.net.ServerSocket(0)) {
			return socket.getLocalPort();
		}
	}
}
