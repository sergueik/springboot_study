package example.config;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.jasypt.encryption.StringEncryptor;

import org.jasypt.properties.EncryptableProperties;
import org.jasypt.encryption.pbe.PooledPBEStringEncryptor;
import org.jasypt.encryption.pbe.config.SimpleStringPBEConfig;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import java.io.File;
import java.io.FileReader;
import java.io.IOException;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;

import java.util.regex.Pattern;

// based on: https://github.com/ulisesbocchio/jasypt-spring-boot/tree/master#use-you-own-custom-encryptor
@Configuration
public class JasyptConfig {

	@Value("${trim:false}")
	private boolean trim;

	private Log logger = LogFactory.getLog(this.getClass());
	private final static Pattern pattern = Pattern.compile("\\s+",
			Pattern.MULTILINE);

	@Value("${jasypt.encryptor.algorithm:PBEWITHHMACSHA512ANDAES_256}")
	private String algorithm;

	@Value("${jasypt.encryptor.iterations:1000}")
	private String iterations;

	@Bean(name = "jasyptStringEncryptor")
	public StringEncryptor stringEncryptor() {
		PooledPBEStringEncryptor encryptor = new PooledPBEStringEncryptor();
		SimpleStringPBEConfig config = new SimpleStringPBEConfig();
		// NOTE: can read password from resource embedded in the application jar
		// instead
		String password = readString(String.format("%s/src/main/resources/%s",
				System.getProperty("user.dir"), "key.txt"));
		// trimming it here too.
		if (trim)
			password = pattern.matcher(password).replaceAll("");
		logger.info("Read: \"" + password + "\"");
		config.setPassword(password);
		config.setAlgorithm(algorithm);
		config.setKeyObtentionIterations(iterations);
		config.setPoolSize("1");
		config.setProviderName("SunJCE");
		config.setSaltGeneratorClassName("org.jasypt.salt.RandomSaltGenerator");
		config.setIvGeneratorClassName("org.jasypt.iv.RandomIvGenerator");
		config.setStringOutputType("base64");
		encryptor.setConfig(config);
		return encryptor;
	}
	// origin: http://www.java2s.com/Code/Java/File-Input-Output/Readatextfile.htm

	public String readString(String filePath) {
		StringBuffer contents = new StringBuffer();
		BufferedReader reader = null;
		String text = null;
		try {

			File file = new File(filePath);
			logger.info("loading " + filePath);

			reader = new BufferedReader(new FileReader(file));

			// repeat until all lines is read, keep line separators
			while ((text = reader.readLine()) != null) {
				contents.append(text).append(System.getProperty("line.separator"));
			}

			text = contents.toString();
			logger
					.info("Read raw data: \"" + text + "\" " + text.length() + " chars");
			if (trim) {
				text = text.split("\r?\n")[0].replaceAll("\\s\\s*", "");

			}
			logger
					.info("Trimmed data: \"" + text + "\": " + text.length() + " chars");
			return text;

		} catch (IOException e) {
			logger.info("Failed to read: " + filePath);
			return "secret";
		} finally {
			try {
				reader.close();
			} catch (IOException e) {
				// ignore
			}
		}
	}

}
