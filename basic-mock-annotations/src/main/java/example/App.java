package example;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

@Service
public class App {

	@Value("${secure.value}")
	private String value;

	public void getValue() {
		System.out.println("value: " + value);
	}

}
