package example.utils;

import java.nio.file.Path;
import java.nio.file.Paths;

import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import me.desair.tus.server.upload.UploadInfo;

@Component
public class TusStorageResolver {
	@Value("${tus.server.data.directory}")
	private String tusDirectory;

	public Path resolve(UploadInfo info) {
		return Paths.get(tusDirectory, "uploads", info.getId().toString(), "data");
		// NOTE: "uploads", "data" are an implementation detail of the library
	}
}
