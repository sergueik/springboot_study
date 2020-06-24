package example;

// import static org.junit.Assert.assertEquals;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.apache.commons.codec.EncoderException;
import com.urbancode.ud.client.AgentClient;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

public class BasicAgentClientTest {
	private static boolean debug = false;
	private static CommandLineParser commandLineParser;

	private static AgentClient client;
	private static JSONObject data;

	public static void main(String[] args) throws URISyntaxException, IOException, JSONException {
		commandLineParser = new CommandLineParser();
		commandLineParser.saveFlagValue("user");
		commandLineParser.saveFlagValue("password");
		commandLineParser.saveFlagValue("agent");

		commandLineParser.parse(args);

		if (commandLineParser.hasFlag("debug")) {
			debug = true;
		}
		String user = commandLineParser.getFlagValue("user");
		if (user == null) {
			System.err.println("Missing required argument: user - assuming default");
			user = "admin";
			// return;
		}
		String password = commandLineParser.getFlagValue("password");
		if (password == null) {
			System.err.println("Missing required argument: password - assuming default");
			password = "admin";
			// return;
		}

		String agent = commandLineParser.getFlagValue("agent");
		if (agent == null) {
			System.err.println("Missing required argument: agent");
			return;
		}
		client = new AgentClient(new URI("https://localhost:8443"), user, password);
		if (client == null) {
			throw new RuntimeException(String.format("failed to connect as %s / password %s", user, password));
		} else {
			data = client.getAgent(agent);
		}
	}
}
