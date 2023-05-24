package example.property;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.properties.ConfigurationProperties;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

// TODO: the prefix no longer work after switching from "file.upload-dir" 
// @ConfigurationProperties(prefix = "file")
@ConfigurationProperties
public class FileStorageProperties {
	private static final Logger logger = LoggerFactory
			.getLogger(FileStorageProperties.class);

	protected String osName = null;

	@Value("${file.upload-dir-windows}")
	private String uploadDirWindows;
	@Value("${file.upload-dir-linux}")
	private String uploadDirLinux;

	private String uploadDir = null;

	public String getUploadDir() {

		if (uploadDir == null) {
			osName = getOSName();
			uploadDir = osName.equals("windows") ? uploadDirWindows : uploadDirLinux;
		}
		logger.info("UploadDir: " + uploadDir);
		return uploadDir;
	}

	public void setUploadDir(String value) {
		uploadDir = value;
	}

	// Utilities
	public String getOSName() {
		if (osName == null) {
			osName = System.getProperty("os.name").toLowerCase();
			if (osName.startsWith("windows")) {
				osName = "windows";
			}
		}
		return osName;
	}
}
