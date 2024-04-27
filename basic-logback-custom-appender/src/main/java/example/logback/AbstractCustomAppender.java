package example.logback;

import ch.qos.logback.classic.PatternLayout;
import ch.qos.logback.core.Context;
import ch.qos.logback.core.Layout;
import ch.qos.logback.core.UnsynchronizedAppenderBase;
import org.apache.http.HttpResponse;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.nio.charset.Charset;

public abstract class AbstractCustomAppender<E> extends UnsynchronizedAppenderBase<E> {

	public static final String DEFAULT_LISTENER_HOSTNAME = "192.168.99.100";
	public static final String DEFAULT_LISTENER_URI = "/cgi-bin/echo_json.cgi";
	public static final String HTTP_LISTENER_PROTOCOL = "http";
	public static final String HTTPS_LISTENER_PROTOCOL = "https";
	public static final String DEFAULT_HTTP_LISTENER_PORT = "9090";
	public static final String DEFAULT_HTTPS_LISTENER_PORT = "9443";

	public static final String DEFAULT_LAYOUT_PATTERN = "%d{\"yyyy-MM-dd'T'HH:mm:ss.SSS'Z'\",UTC} %-5level [%thread] %logger: %m%n";

	protected static final Charset UTF_8 = Charset.forName("UTF-8");

	protected String hostname;
	protected String uri;
	protected String protocol;
	protected String port;
	protected String token;
	private String endpoint;

	protected Layout<E> layout;
	protected boolean layoutCreated = false;
	private String pattern;

	@Override
	public void start() {
		ensureLayout();

		if (!this.layout.isStarted()) {
			this.layout.start();
		}

		// set protocol first, so port can be detected
		ensureProtocol();

		ensurePort();

		if (this.hostname == null) {
			this.hostname = DEFAULT_LISTENER_HOSTNAME;
		}

		if (this.uri == null) {
			this.uri = DEFAULT_LISTENER_URI;
		}

		if (this.token == null) {
			addError("Token must be configured");
		}

		buildEndpoint();

		super.start();
	}

	@Override
	public void stop() {
		super.stop();
		if (this.layoutCreated) {
			try {
				this.layout.stop();
			} finally {
				this.layout = null;
				this.layoutCreated = false;
			}
		}
	}

	protected void ensureProtocol() {
		if (protocol == null) {
			protocol = HTTP_LISTENER_PROTOCOL;
			return;
		}

		if (protocol != HTTP_LISTENER_PROTOCOL || protocol != HTTPS_LISTENER_PROTOCOL) {
			addError("Invalid protocol: " + protocol);
		}
	}

	protected void ensurePort() {
		if (this.port == null) {
			if (getProtocol() == HTTPS_LISTENER_PROTOCOL) {
				this.port = DEFAULT_HTTPS_LISTENER_PORT;
			}

			this.port = DEFAULT_HTTP_LISTENER_PORT;
		}
	}

	protected final void ensureLayout() {
		if (this.layout == null) {
			this.layout = createLayout();
			this.layoutCreated = true;
		}

		if (this.layout != null) {
			Context context = this.layout.getContext();
			if (context == null) {
				this.layout.setContext(getContext());
			}
		}
	}

	@SuppressWarnings("unchecked")
	protected Layout<E> createLayout() {
		PatternLayout layout = new PatternLayout();
		String pattern = getPattern();

		if (pattern == null) {
			pattern = DEFAULT_LAYOUT_PATTERN;
		}

		layout.setPattern(pattern);

		return (Layout<E>) layout;
	}

	protected String buildEndpoint() {
		StringBuilder result = new StringBuilder();

		String protocol = removePostfix(this.protocol, "://");
		String hostname = removePostfix(this.hostname, "/");
		String uri = removePostfix(this.uri, "/");

		// endpoint trailing slash
		// if (uri != "") {
		// uri = uri + '/';
		// }

		endpoint = result.append(protocol).append("://").append(hostname).append(":").append(port).append(uri)
				.append('?').append("token").append("=").append(token).toString();

		return endpoint;
	}

	protected String removePostfix(String s, String postfix) {
		if (s == null)
			return s;

		return s.substring(0, s.length() - (s.endsWith(postfix) ? postfix.length() : 0));
	}

	protected String cleanString(String data) {
		if (data != null) {
			data = data.trim();
		}
		if ("".equals(data)) {
			data = null;
		}

		return data;
	}

	protected String readResponseBody(HttpResponse response) throws IOException {
		BufferedReader rd = new BufferedReader(new InputStreamReader(response.getEntity().getContent()));

		StringBuffer result = new StringBuffer();
		String line = "";

		while ((line = rd.readLine()) != null) {
			result.append(line);
		}

		return result.toString();
	}

	public String getProtocol() {
		return protocol;
	}

	public void setProtocol(String value) {
		protocol = cleanString(value);
	}

	public String getHostname() {
		return hostname;
	}

	public void setHostname(String value) {
		hostname = cleanString(value);
	}

	public String getUri() {
		return uri;
	}

	public void setUri(String value) {
		uri = cleanString(value);
	}

	public String getPort() {
		return port;
	}

	public void setPort(String value) {
		port = cleanString(value);
	}

	public String getToken() {
		return token;
	}

	public void setToken(String value) {
		token = cleanString(value);
	}

	public String getEndpoint() {
		return endpoint;
	}

	public String getPattern() {
		return pattern;
	}

	public void setPattern(String value) {
		pattern = value;
	}

	public Layout<E> getLayout() {
		return layout;
	}

	public void setLayout(Layout<E> value) {
		layout = value;
	}

}
