package example.service;

import org.springframework.stereotype.Service;

import java.util.List;
import java.util.stream.Collectors;

import org.springframework.http.converter.json.MappingJackson2HttpMessageConverter;

public class MessageServiceImpl implements MessageService {

	public String sendData(List<Object> data) {
		return String.join(",", data.stream().map(o -> o.toString()).collect(Collectors.toList()));
	}

}
