package example.service;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.core.io.Resource;
import org.springframework.stereotype.Service;

import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;

@Service
public class JsonAssembler {

	private final ObjectMapper mapper = new ObjectMapper();

	@Value("${app.components.customer}")
	private Resource customer;

	@Value("${app.components.account}")
	private Resource account;

	@Value("${app.components.transaction}")
	private Resource transaction;

	public JsonNode assemble() throws Exception {

		ObjectNode root = (ObjectNode) mapper.readTree(transaction.getInputStream());
		JsonNode customerNode = mapper.readTree(customer.getInputStream());
		JsonNode accountNode = mapper.readTree(account.getInputStream());

		root.set("customer", customerNode);
		root.set("account", accountNode);

		return root;
	}
}
