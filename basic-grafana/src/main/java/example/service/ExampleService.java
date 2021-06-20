package example.service;

import java.util.HashMap;
import java.util.Map;

import org.springframework.stereotype.Service;

@Service
// NOTE: should not use reserved name "Service" when naming the class :
// collision with stereotype:
// incompatible types: example.Service cannot be converted to
// java.lang.annotation.Annotation

public class ExampleService {

	public Map<String, Object> getDataMap(String data) {
		return new HashMap<String, Object>();
	}
}