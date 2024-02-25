package example.resources;

import com.google.gson.Gson;

import example.entity.Config;
import example.listener.ExecutorTailor;
import example.listener.TailerListenerAdapter;
import example.notification.ClientNotification;
import example.notification.WordNotification;
import example.repository.ConfigDao;

import org.apache.commons.io.input.Tailer;
import org.apache.commons.io.input.TailerListener;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.util.CollectionUtils;
import org.springframework.web.bind.annotation.*;

import javax.ws.rs.Consumes;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;
import javax.xml.ws.Response;
import java.util.List;
import java.util.Optional;
import java.util.concurrent.Callable;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

@RestController
@RequestMapping("/tailer")
@Consumes(MediaType.APPLICATION_JSON)
@Produces(MediaType.APPLICATION_JSON)
public class TailerResource {

	private WordNotification wordNotification;
	private final ClientNotification clientNotification;
	private final TailerListenerAdapter tailerListener;
	private final ConfigDao configDao;

	@Autowired
	public TailerResource(final WordNotification wordNotification,
			final ClientNotification clientNotification,
			final TailerListenerAdapter tailerListener, final ConfigDao configDao) {
		this.wordNotification = wordNotification;
		this.clientNotification = clientNotification;
		this.tailerListener = tailerListener;
		this.configDao = configDao;
	}

	@GetMapping(path = "/config", produces = { MediaType.APPLICATION_JSON })
	@ResponseStatus(HttpStatus.ACCEPTED)
	public String getConfig() {
		if (this.getConfigGravado().isPresent()) {
			return new Gson().toJson(this.getConfigGravado().get());
		}
		return "{Config not found}";
	}

	@GetMapping(path = "/config/words", produces = { MediaType.APPLICATION_JSON })
	@ResponseStatus(HttpStatus.ACCEPTED)
	public String getPalavras() {
		if (this.getConfigGravado().isPresent()
				&& this.getConfigGravado().get().getWord() != null) {
			return new Gson()
					.toJson(this.getConfigGravado().get().getWord().getWords());
		}
		return "No words recorded";
	}

	@PutMapping(path = "/config", produces = { MediaType.APPLICATION_JSON })
	@ResponseStatus(HttpStatus.ACCEPTED)
	public String novaConfig(@RequestBody String novaConfig) {
		Config configJson = new Gson().fromJson(novaConfig, Config.class);

		List<Config> configList = this.configDao.findAll();
		if (!CollectionUtils.isEmpty(configList)) {
			Config config = configList.get(0);
			configJson.setId(config.getId());
		}
		this.configDao.save(configJson);
		return "OK";
	}

	@GetMapping(path = "/start", produces = { MediaType.APPLICATION_JSON })
	@ResponseStatus(HttpStatus.ACCEPTED)
	public String startTailer() {
		Optional<Config> configGravado = getConfigGravado();
		if (configGravado.isPresent()) {
			Config config = configGravado.get();
			this.wordNotification.setMessage(config.getWord().getMessage());
			this.wordNotification.setWords(config.getWord().getWords());

			if (config.getFile() == null) {
				return "File not configured";
			}

			if (config.getWord() == null) {
				return "No words recorded";
			}

			if (config.getWord() != null && config.getWord().getWords().isEmpty()) {
				return "No words recorded";
			}

			if (ExecutorTailor.getInstance().existeTailer()) {
				ExecutorTailor.getInstance().tryStartTailer();
				Config config1 = this.configDao.findAll().get(0);

				Executors.newFixedThreadPool(1).execute(() -> {
					System.out.println(Thread.currentThread().getName() + " - "
							+ Thread.currentThread().getId());
					ExecutorTailor.getInstance().init(config1.getFile(), wordNotification,
							clientNotification, tailerListener, config);
				});
			} else {
				Executors.newFixedThreadPool(1).execute(() -> {
					System.out.println(Thread.currentThread().getName() + " - "
							+ Thread.currentThread().getId());
					ExecutorTailor.getInstance().init(config.getFile(), wordNotification,
							clientNotification, tailerListener, config);
				});
			}
		}

		return "OK";
	}

	@GetMapping(path = "/status", produces = { MediaType.APPLICATION_JSON })
	@ResponseStatus(HttpStatus.ACCEPTED)
	public String getStatus() {
		return this.tailerListener.getStatus();
	}

	private Optional<Config> getConfigGravado() {
		if (CollectionUtils.isEmpty(this.configDao.findAll())) {
			return Optional.empty();
		}
		return Optional.of(this.configDao.findAll().get(0));
	}

}
