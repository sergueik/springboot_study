package example.notification;

import com.google.gson.Gson;

import example.entity.Config;
import example.repository.ConfigDao;

import org.glassfish.jersey.internal.guava.Lists;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.*;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import org.springframework.web.servlet.support.ServletUriComponentsBuilder;

import java.util.Arrays;
import java.util.Collections;

@Service
public class ClientNotification {

	private final ConfigDao configDao;
	private final RestTemplate restTemplate;

	@Autowired
	public ClientNotification(final ConfigDao configDao,
			final RestTemplate restTemplate) {
		this.configDao = configDao;
		this.restTemplate = restTemplate;
	}

	public void notificaPalavraEncontrada(final String word, final String line) {
		Config config = this.configDao.findAll().get(0);

		HttpHeaders httpHeaders = new HttpHeaders();
		httpHeaders
				.setAccept(Collections.singletonList(MediaType.APPLICATION_JSON));

		HttpEntity<Notification> httpEntity = new HttpEntity<>(
				new Notification(word, line, config.getFile()), httpHeaders);
		restTemplate.postForEntity(config.getUrl(), httpEntity, String.class);
	}
}
