package example.service;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Service;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import me.desair.tus.server.upload.UploadInfo;

import example.utils.TusStorageResolver;

@Service
public class FinalizeService {
	private static final Logger logger = LoggerFactory.getLogger(FinalizeService.class);
	@Autowired
	private TusStorageResolver tusStorageResolver;

	public void finalizeUpload(UploadInfo info) throws IOException {
		String uploadId = info.getId().toString();
		Path targetFilePath = Paths.get(String.format("%s%starget%sdata%s%s", System.getProperty("user.dir"),
				File.separator, File.separator, File.separator, uploadId));
		Files.createDirectories(targetFilePath.getParent());
		logger.info("created target file path: {}", targetFilePath.toString());
		Path inputFilePath = tusStorageResolver.resolve(info).toAbsolutePath();
		logger.info("move {} to {}", inputFilePath, targetFilePath);
		Files.move(inputFilePath, targetFilePath, StandardCopyOption.REPLACE_EXISTING);
	}
}
