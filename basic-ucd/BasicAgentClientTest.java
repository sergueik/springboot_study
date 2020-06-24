// package com.urbancode.ud.client;

// import static org.junit.Assert.assertEquals;
import java.io.IOException;
import java.net.URI;
import java.net.URISyntaxException;

import org.apache.commons.codec.EncoderException;
import com.urbancode.ud.client.AgentClient;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

// import org.junit.Before;
// import org.junit.Test;

public class BasicAgentClientTest {

    private static AgentClient client;
private static JSONObject data;

    public static void main(String[] args) throws URISyntaxException,IOException, JSONException {
        client = new AgentClient(new URI("https://localhost:8443"), "admin", "admin");
	if (client == null ) {  
           throw new RuntimeException( "failed to connect");
        } else{ 
data = client.getAgent("dummy");
}

    }
}
