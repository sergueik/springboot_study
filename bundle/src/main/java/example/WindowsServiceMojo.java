package example;

import org.apache.maven.plugin.AbstractMojo;
import org.codehaus.plexus.util.FileUtils;
import org.codehaus.plexus.util.StringUtils;
import org.dom4j.Document;
import org.dom4j.Element;
import org.dom4j.io.SAXReader;
import org.dom4j.io.XMLWriter;

import java.io.*;
import java.net.URL;
import java.nio.charset.StandardCharsets;

public class WindowsServiceMojo extends AbstractMojo {
	private File targetDir;
	private File baseDir;
	private File sourceDir;
	private File testSourceDir;
	private String groupId;
	private String artifactId;
	private String version;
	private String description;
	private String[] arguments;
	private String vmOptions;
	private String programArguments;
	private static final String baseUrl = "http://image.joylau.cn/plugins/joylau-springboot-daemon-windows";

	private static String EXE_FILE_URL = String.format("%s/service.exe", baseUrl);
	private static String XML_FILE_URL = String.format("%s/service.xml", baseUrl);
	private static String CONFIG_FILE_URL = String.format("%s/service.exe.config", baseUrl);
	private static final String README_FILE_URL = String.format("%s/readme.txt", baseUrl);

	public void execute() {
		getLog().info("\n" + "Start generating Windows Service required files");
		try {
			File distDir = new File(targetDir, File.separator + "dist");
			if (distDir.exists()) {
				try {
					FileUtils.deleteDirectory(distDir);
				} catch (IOException e) {
					getLog().error("Cannot remove directory: " + e.toString());
				}
			}
			FileUtils.mkdir(distDir.getPath());
			File logDir = new File(distDir, File.separator + "logs");
			FileUtils.mkdir(logDir.getPath());

			FileUtils.copyURLToFile(new URL(README_FILE_URL), new File(distDir, File.separator + "readme.txt"));
			FileUtils.copyURLToFile(new URL(XML_FILE_URL),
					new File(distDir, File.separator + getJarPrefixName() + ".xml"));
			FileUtils.copyURLToFile(new URL(EXE_FILE_URL),
					new File(distDir, File.separator + getJarPrefixName() + ".exe"));
			FileUtils.copyURLToFile(new URL(CONFIG_FILE_URL),
					new File(distDir, File.separator + getJarPrefixName() + ".exe.config"));
			FileUtils.copyFile(new File(targetDir.getPath() + File.separator + getJarName()),
					new File(distDir, File.separator + getJarName()));

			convert(new File(distDir.getPath() + File.separator + getJarPrefixName() + ".xml"));
			createBat(distDir, "install.bat", "install");
			createBat(distDir, "uninstall.bat", "uninstall");
			createBat(distDir, "start.bat", "start");
			createBat(distDir, "stop.bat", "stop");
			createBat(distDir, "restart.bat", "restart");

			getLog().info("Compressing the bundle");
			String zipDir = targetDir.getPath() + File.separator + getJarPrefixName() + ".zip";
			ZipUtils.zip(distDir.getPath(), zipDir);

			getLog().info("Removing temporary file");
			FileUtils.deleteDirectory(distDir);
			getLog().info("File is ready:" + zipDir);
		} catch (Exception e) {
			getLog().error("Failed with exception：", e);
		}
	}

	private void convert(File xmlFile) {
		SAXReader reader = new SAXReader();
		try {
			Document document = reader.read(xmlFile);
			Element root = document.getRootElement();
			root.element("id").setText(artifactId);
			root.element("name").setText(getJarPrefixName());
			root.element("description").setText(null == description ? "No description" : description);
			if (arguments.length > 0) {
				getLog().warn(
						"Invalid arguments. Please set the vmOptions parameter and the programArguments parameters");
			}
			String vm_options = StringUtils.isEmpty(vmOptions) ? " " : " " + vmOptions + " ";
			String program_arguments = StringUtils.isEmpty(programArguments) ? "" : " " + programArguments;
			root.element("arguments").setText(vm_options + "-jar " + getJarName() + program_arguments);
			saveXML(document, xmlFile);
		} catch (Exception e) {
			e.printStackTrace();
		}
	}

	private void saveXML(Document document, File xmlFile) {
		try {
			XMLWriter writer = new XMLWriter(
					new OutputStreamWriter(new FileOutputStream(xmlFile), StandardCharsets.UTF_8));
			writer.write(document);
			writer.flush();
			writer.close();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}

	private void createBat(File outDri, String fileName, String text) {
		if (!outDri.exists()) {
			FileUtils.mkdir(outDri.getPath());
		}
		File file = new File(outDri, fileName);
		try (FileWriter w = new FileWriter(file)) {
			//
			w.write("@echo off\n"
					+ "%1 mshta vbscript:CreateObject(\"Shell.Application\").ShellExecute(\"cmd.exe\",\"/c %~s0 ::\",\"\",\"runas\",1)(window.close)&&exit\n"
					+ "%~dp0" + getJarPrefixName() + ".exe " + text + "\n" + "echo The " + getJarPrefixName()
					+ " service current state:\n" + "%~dp0" + getJarPrefixName() + ".exe status\n" + "pause");
		} catch (IOException e) {
//            throw new MojoExecutionException("Error creating file ", e);
			e.printStackTrace();
		}
		// ignore
	}

	private String getJarPrefixName() {
		return artifactId + "-" + version;
	}

	private String getJarName() {
		return getJarPrefixName() + ".jar";
	}
}
