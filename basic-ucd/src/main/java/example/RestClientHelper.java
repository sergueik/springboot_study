package example;

import java.io.File;
import java.io.IOException;
import java.io.Serializable;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.net.URI;
import java.util.UUID;

import org.apache.http.impl.client.DefaultHttpClient;

import org.codehaus.jettison.json.JSONArray;
import org.codehaus.jettison.json.JSONException;
import org.codehaus.jettison.json.JSONObject;

import com.urbancode.ud.client.ApplicationClient;
import com.urbancode.ud.client.ComponentClient;
import com.urbancode.ud.client.PropertyClient;
import com.urbancode.ud.client.SystemClient;
import com.urbancode.ud.client.VersionClient;
import com.urbancode.ud.client.UDRestClient;

public class RestClientHelper implements Serializable {
	private URI ucdUrl;
	private UrbanDeploySite udSite;
	private String altUser;
	private String altPassword;

	public RestClientHelper(URI ucdUrl, UrbanDeploySite udSite, String altUser,
			String altPassword) {
		this.ucdUrl = ucdUrl;
		this.udSite = udSite;
		this.altUser = altUser != null ? altUser.trim() : "";
		this.altPassword = altPassword;
	}

	/**
	 * Creates the component version
	 *
	 * @return UUID of the new version
	 *
	 * @throws AbortException
	 */
	public UUID createComponentVersion(String version, String component,
			String description) throws Exception {
		VersionClient versionClient = new VersionClient(ucdUrl, getUdClient());

		if (version == null || version.isEmpty() || version.length() > 255) {
			throw new Exception(
					String.format("Failed to create version '%s' in UrbanCode Deploy. "
							+ "UrbanCode Deploy version names' length must be between 1 and  255 characters "
							+ "long. (Current length: %s)", version, version.length()));
		}

		UUID versionId;

		versionId = versionClient.createVersion(component, version, description);

		return versionId;
	}

	public void uploadVersionFiles(File workDir, String component, String version,
			String includePatterns, String excludePatterns) throws Exception {
		VersionClient versionClient = new VersionClient(ucdUrl, getUdClient());
		String[] includes = splitFiles(includePatterns);
		String[] excludes = splitFiles(excludePatterns);

		versionClient.addVersionFiles(component, version, workDir, "", includes,
				excludes, true, true);
	}

	public void deleteComponentVersion(UUID id) throws Exception {
		VersionClient versionClient = new VersionClient(ucdUrl, getUdClient());

		versionClient.deleteVersion(id);
	}

	public void addLinkToComp(String compName, String versionName,
			String linkName, String linkUrl) throws Exception {
		ComponentClient compClient = new ComponentClient(ucdUrl, getUdClient());
		compClient.addComponentVersionLink(compName, versionName, linkName,
				linkUrl);
	}

	public String checkDeploymentProcessResult(String procId) throws Exception {
		ApplicationClient appClient = new ApplicationClient(ucdUrl, getUdClient());
		String deploymentResult;

		deploymentResult = appClient.getApplicationProcessStatus(procId);

		return deploymentResult;
	}

	private Map<String, String> readProperties(String properties)
			throws Exception {
		Map<String, String> propertiesToSet = new HashMap<String, String>();
		System.out.println("Properties: " + properties);
		if (properties != null && properties.length() > 0) {
			for (String line : properties.split("\n")) {
				String[] propDef = line.split("=");

				if (propDef.length >= 2) {
					String propName = propDef[0].trim();
					String propVal = propDef[1].trim();
					propertiesToSet.put(propName, propVal);
				} else {
					throw new Exception(
							"Missing property delimiter '=' in property definition '" + line
									+ "'");
				}
			}
		}
		return propertiesToSet;
	}

	/**
	 * Split String of filenames by newline and remove empty/null entries
	 * @param Newline separated list of filenames
	 * @return Array of filenames
	 */
	private String[] splitFiles(String patterns) {
		List<String> newList = new ArrayList<String>();

		String[] patternList = patterns.split("\n");

		for (String pattern : patternList) {
			if (pattern != null && pattern.trim().length() > 0) {
				newList.add(pattern.trim());
			}
		}

		return newList.toArray(new String[newList.size()]);
	}

	private DefaultHttpClient getUdClient() {
		DefaultHttpClient udClient;

		if (altUser.isEmpty()) {
			udClient = udSite.getClient();
		} else {
			udClient = udSite.getTempClient(altUser, altPassword);
		}

		return udClient;
	}
}
