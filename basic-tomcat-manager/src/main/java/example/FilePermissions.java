package example;

import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.attribute.PosixFileAttributeView;
import java.nio.file.attribute.PosixFileAttributes;
import java.nio.file.attribute.PosixFilePermission;
import java.nio.file.attribute.PosixFilePermissions;
import java.util.Set;

// origin: http://www.java2s.com/Tutorials/Java/Java_io/1030__Java_nio_File_Owner_Permissions.htm
// see also: https://github.com/apache/logging-log4j2/blob/master/log4j-core/src/test/java/org/apache/logging/log4j/core/appender/rolling/RollingRandomAccessFileManagerTest.java
public class FilePermissions {
	private static Set<PosixFilePermission> permissions;
	private static PosixFileAttributeView posixView;
	private static String posixFilePermissions;

	public static void main(String[] argv) throws Exception {
		if (argv.length == 0) {
			return;
		}
		Path filePath = Paths.get(argv[0]);
		posixView = Files.getFileAttributeView(filePath,
				PosixFileAttributeView.class);
		PosixFileAttributes posixFileAttributes = posixView.readAttributes();
		permissions = posixFileAttributes.permissions();
		posixFilePermissions = PosixFilePermissions.toString(permissions);
		System.out.println("Current permissons: " + posixFilePermissions);
		posixFilePermissions = argv.length > 1 ? argv[1] : "rw-r-----";
		// observe no effect of umask
		permissions = PosixFilePermissions.fromString(posixFilePermissions);
		posixView.setPermissions(permissions);
	}
}
