package example.service;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;

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
		mkdirs(targetFilePath.toFile());
		Path inputFilePath = tusStorageResolver.resolve(info).toAbsolutePath();
		logger.info("move {} to {}", inputFilePath, targetFilePath);
		Files.move(inputFilePath, targetFilePath, StandardCopyOption.REPLACE_EXISTING);
	}

	public static void mkdirs(File dir) {
		File parent = dir.getAbsoluteFile();
		List<File> mkdir = new ArrayList<File>();
		for (; !parent.exists() || !parent.isDirectory(); parent = parent.getParentFile()) {
			mkdir.add(parent);
		}
		for (int i = mkdir.size(); --i >= 0;) {
			File d = mkdir.get(i);
			d.mkdir();
			d.setReadable(true, false);
			d.setWritable(true, false);
		}
	}

}
