package example.listener;

import lombok.RequiredArgsConstructor;
import org.apache.commons.io.input.Tailer;
import org.apache.commons.io.input.TailerListener;
import org.apache.commons.lang3.BooleanUtils;
import org.apache.commons.lang3.StringUtils;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ReflectionUtils;

import example.notification.ClientNotification;
import example.notification.WordNotification;

import javax.inject.Named;
import java.lang.reflect.Method;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
public final class TailerListenerImpl implements TailerListenerAdapter {

	private final WordNotification wordNotification;
	private final ClientNotification clientNotification;

	private Tailer tailers;

	@Autowired
	public TailerListenerImpl(final WordNotification wordNotification,
			final ClientNotification clientNotification) {
		this.wordNotification = wordNotification;
		this.clientNotification = clientNotification;
	}

	@Override
	public void init(final Tailer tailer) {
		this.tailers = tailer;
	}

	@Override
	public void fileNotFound() {
		// uma forma de apresentar o status
		System.out.println("rquivo nao encontrado");
	}

	@Override
	public void fileRotated() {

	}

	@Override
	public void handle(final String line) {
		if (wordNotification != null) {
			for (final String word : this.wordNotification.getWords()) {
				final boolean contemPalavra = StringUtils.containsIgnoreCase(line,
						word);
				if (contemPalavra) {
					clientNotification.notificaPalavraEncontrada(word, line);
				}
			}
		}
	}

	@Override
	public void handle(final Exception e) {
		// mostrear o status
		System.out.println(e);

		this.tailers.run();
	}

	@Override
	public String getStatus() {
		if (this.tailers == null) {
			return "{\"Nenhum Monitoramento ativado\"}";
		}

		StringBuilder sb = new StringBuilder();
		boolean running = isRunning(this.tailers);
		String status = running ? "\"Running\"" : "\"Stopped\"";
		sb.append("{\"Arquivo\": ").append("\"")
				.append(this.tailers.getFile().getAbsolutePath()).append("\"")
				.append(", \"Status\": ").append(status).append(System.lineSeparator());
		return sb.toString();
	}

	@Override
	public boolean isRunning(final Tailer tailer) {
		Method getRun = ReflectionUtils.findMethod(Tailer.class, "getRun");
		ReflectionUtils.makeAccessible(getRun);
		Boolean o = (Boolean) ReflectionUtils.invokeMethod(getRun, tailer);
		return o;
	}
}
