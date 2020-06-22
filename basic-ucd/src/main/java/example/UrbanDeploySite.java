package example;

import com.urbancode.commons.util.IO;
import com.urbancode.ud.client.UDRestClient;

import java.io.IOException;
import java.io.InputStream;
import java.io.Reader;
import java.io.Serializable;
import java.net.URI;
import java.net.URISyntaxException;

import javax.ws.rs.core.UriBuilder;

import org.apache.commons.lang3.StringUtils;
import org.apache.http.client.HttpClient;
import org.apache.http.client.methods.HttpGet;
import org.apache.http.HttpResponse;
import org.apache.http.impl.client.DefaultHttpClient;

/**
 * This class creates an HttpClient for running UCD rest commands
 *
 */
@SuppressWarnings("deprecation") // Triggered by DefaultHttpClient
public class UrbanDeploySite implements Serializable {

	private static final long serialVersionUID = -8723534991244260459L;

	private String profileName;

	private String url;

	private String user;

	private String password;

	private boolean trustAllCerts;

	transient private DefaultHttpClient client;

	/**
	 * Instantiates a new UrbanDeploy site.
	 */
	public UrbanDeploySite() {
	}

	/**
	 * Necessary constructor to allow jenkins to treat the password as an encrypted value
	 *
	 * @param profileName
	 * @param url the url of the UrbanDeploy instance
	 * @param user
	 * @param password
	 * @param trustAllCerts
	 */
	public UrbanDeploySite(String profileName, String url, String user,
			String password, boolean trustAllCerts) {
		this.profileName = profileName;
		this.url = url;
		this.user = user;
		this.password = password;
		this.trustAllCerts = trustAllCerts;
	}

	public DefaultHttpClient getClient() {
		if (client == null) {
			client = UDRestClient.createHttpClient(user, password.toString(),
					trustAllCerts);
		}

		return client;
	}

	public DefaultHttpClient getTempClient(String tempUser, String tempPassword) {
		return UDRestClient.createHttpClient(tempUser, tempPassword.toString(),
				trustAllCerts);
	}

	/**
	 * Gets the display name.
	 *
	 * @return the display name
	 */
	public String getDisplayName() {
		if (StringUtils.isEmpty(profileName)) {
			return url;
		} else {
			return profileName;
		}
	}

	/**
	 * Gets the profile name.
	 *
	 * @return the profile name
	 */
	public String getProfileName() {
		return profileName;
	}

	/**
	 * Sets the profile name.
	 *
	 * @param profileName
	 *          the new profile name
	 */
	public void setProfileName(String profileName) {
		this.profileName = profileName;
	}

	/**
	 * Gets the url.
	 *
	 * @return the url
	 */
	public String getUrl() {
		return url;
	}

	/**
	 * Sets the url.
	 *
	 * @param url
	 *          the new url
	 */
	public void setUrl(String url) {
		this.url = url;
		if (this.url != null) {
			this.url = this.url.replaceAll("\\\\", "/");
		}
		while (this.url != null && this.url.endsWith("/")) {
			this.url = this.url.substring(0, this.url.length() - 2);
		}
	}

	public URI getUri() throws URISyntaxException {
		URI udSiteUri;

		udSiteUri = new URI(url);

		return udSiteUri;
	}

	/**
	 * Gets the username.
	 *
	 * @return the username
	 */
	public String getUser() {
		return user;
	}

	/**
	 * Sets the username.
	 *
	 * @param username
	 *          the new username
	 */
	public void setUser(String user) {
		this.user = user;
	}

	/**
	 * Gets the password.
	 *
	 * @return the password
	 */
	public String getPassword() {
		return password;
	}

	/**
	 * Sets the password.
	 *
	 * @param password
	 *          the new password
	 */
	public void setPassword(String password) {
		this.password = password;
	}

	/**
	 * Gets trustAllCerts
	 *
	 * @return if all certificates are trusted
	 */
	public boolean isTrustAllCerts() {
		return trustAllCerts;
	}

	/**
	 * Sets trustAllCerts to trust all ssl certificates or not
	 *
	 * @param trustAllCerts
	 */
	public void setTrustAllCerts(boolean trustAllCerts) {
		this.trustAllCerts = trustAllCerts;
	}

	/**
	 * Test whether the client can connect to the UCD site
	 *
	 * @throws Exception
	 */
	public void verifyConnection() throws Exception {
		URI uri = UriBuilder.fromPath(url).path("rest").path("state").build();
		executeJSONGet(uri);
	}

	/**
	 * Execute an HTTP GET request to the UCD server
	 *
	 * @param uri
	 * @throws Exception
	 */
	private void executeJSONGet(URI uri) throws Exception {
		String result = null;
		HttpClient client = getClient();
		HttpGet method = new HttpGet(uri.toString());
		try {
			HttpResponse response = client.execute(method);
			int responseCode = response.getStatusLine().getStatusCode();
			if (responseCode == 401) {
				throw new Exception(
						"Error connecting to IBM UrbanCode Deploy: Invalid user and/or password");
			} else if (responseCode != 200) {
				throw new Exception("Error connecting to IBM UrbanCode Deploy: "
						+ responseCode + "using URI: " + uri.toString());
			}
		} finally {
			method.releaseConnection();
		}
	}
}