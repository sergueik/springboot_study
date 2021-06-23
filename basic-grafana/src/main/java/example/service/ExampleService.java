package example.service;

import java.util.HashMap;
import java.util.Map;

import org.springframework.stereotype.Service;

@Service
public class ExampleService {

	// currently unused
	public Map<String, Object> getDataMap(String data) {
		return new HashMap<String, Object>();
	}
}