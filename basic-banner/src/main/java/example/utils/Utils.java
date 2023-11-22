package example.utils;

import java.io.InputStream;
import java.util.Properties;

public class Utils {
	private static Utils instance = new Utils();

	private boolean debug = false;

	public void setDebug(boolean value) {
		debug = value;
	}

	private Properties properties = new Properties();
	private InputStream inputStream = null;
	private String resourcePath;

	private Utils() {
	}

	public static Utils getInstance() {
		return instance;
	}

	public synchronized String getVersion(boolean usepom, boolean usemanifest) {
		String version = null;

		if (usepom) {
			// try to load from maven properties straight
			try {
				resourcePath = "maven/example/banner/pom.properties";
				inputStream = getClass()
						.getResourceAsStream("/META-INF/" + resourcePath);
				if (debug)
					System.err.println("reading the resource: " + resourcePath);
				if (inputStream != null) {
					if (debug)
						System.err.println("read the resource: " + resourcePath);
					properties.load(inputStream);
					version = properties.getProperty("version", "");
					if (debug)
						System.err.println("read the version: " + version);
				} else {
					if (debug)
						System.err.println("failed to find the version");
				}
			} catch (Exception e) {
				// ignore
				System.err.println("Exception (ignord):" + e.toString());
			}
		}
		if (usemanifest) {

			try {
				resourcePath = "MANIFEST.MF";
				inputStream = getClass()
						.getResourceAsStream("/META-INF/" + resourcePath);
				if (debug)
					System.err.println("reading the resource: " + resourcePath);
				if (inputStream != null) {
					if (debug)
						System.err.println("read the resource: " + resourcePath);
					properties.load(inputStream);
					version = properties.getProperty("version", "");
					if (debug)
						System.err.println("read the version: " + version);
				} else {
					if (debug)
						System.err.println("failed to find the version");
				}
			} catch (Exception e) {
				// ignore
				System.err.println("Exception (ignord):" + e.toString());
			}
		}
		// fallback to using Java API
		if (version == null)

		{
			Package aPackage = getClass().getPackage();
			if (aPackage != null) {
				version = aPackage.getImplementationVersion();
				if (version == null) {
					version = aPackage.getSpecificationVersion();
				}
			}
		}

		if (version == null) {
			// we could not compute the version so use a blank
			version = "";
		}
		return version;
	}
}
