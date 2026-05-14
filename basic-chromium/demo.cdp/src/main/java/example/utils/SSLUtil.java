package example.utils;

import com.neovisionaries.ws.client.WebSocketFactory;

import javax.net.ssl.HttpsURLConnection;
import javax.net.ssl.SSLContext;
import javax.net.ssl.TrustManager;
import javax.net.ssl.X509TrustManager;
import java.security.KeyManagementException;
import java.security.NoSuchAlgorithmException;
import java.security.cert.X509Certificate;
import java.util.HashMap;
import java.util.Map;

public class SSLUtil {
	private static final TrustManager[] UNQUESTIONING_TRUST_MANAGER = new TrustManager[] {
			new X509TrustManager() {
				public X509Certificate[] getAcceptedIssuers() {
					return null;
				}

				public void checkClientTrusted(X509Certificate[] certs,
						String authType) {
				}

				public void checkServerTrusted(X509Certificate[] certs,
						String authType) {
				}
			} };

	public static void turnOffSslChecking() {
		try {
			// Install the all-trusting trust manager
			final SSLContext sSLContext = SSLContext.getInstance("SSL");
			sSLContext.init(null, UNQUESTIONING_TRUST_MANAGER, null);
			HttpsURLConnection
					.setDefaultSSLSocketFactory(sSLContext.getSocketFactory());
		} catch (Exception e) {
			System.out.println("Error in SSL Utils");
		}
	}

	public static void turnOffSslChecking(WebSocketFactory factory) {
		try {
			// Install the all-trusting trust manager
			final SSLContext sSLContext = SSLContext.getInstance("SSL");
			sSLContext.init(null, UNQUESTIONING_TRUST_MANAGER, null);
			HttpsURLConnection
					.setDefaultSSLSocketFactory(sSLContext.getSocketFactory());
			factory.setSSLContext(sSLContext);
		} catch (Exception e) {
			System.out.println("Error in SSL Utils");
		}
	}

	public static void turnOnSslChecking()
			throws KeyManagementException, NoSuchAlgorithmException {
		// Return it to the initial state (discovered by reflection, now hardcoded)
		SSLContext.getInstance("SSL").init(null, null, null);
	}

	private SSLUtil() {
		throw new UnsupportedOperationException(
				"Should not instantiate libraries.");
	}
}
