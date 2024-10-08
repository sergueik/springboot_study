package example;

/**
 * Copyright 2023,2024 Serguei Kouzmine
 */

import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import com.sun.jna.platform.win32.Advapi32;
import com.sun.jna.platform.win32.Advapi32Util;
import com.sun.jna.platform.win32.Kernel32;
import com.sun.jna.platform.win32.Win32Exception;
import com.sun.jna.platform.win32.WinNT;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinReg;

public class Win32EventLogAppender {

	private static final String EVENT_LOG_PATH = "SYSTEM\\CurrentControlSet\\Services\\EventLog\\";
	private static final String CATEGORY_MESSAGE_FILE = "CategoryMessageFile";
	private static final String EVENT_MESSAGE_FILE = "EventMessageFile";
	private static final int CATEGORY_COUNT = 6;
	private static final int TYPES_SUPPORTED = 7;
	private static final String DEFAULT_SOURCE = "Log4jna";
	private static final String DEFAULT_APPLICATION = "Application";
	private static final int MESSAGE_ID = 1000;
	// final int messageID = 1000 ;
	// final int messageID = 0x1000; // 4096

	private String _source = null;
	private int messageID = MESSAGE_ID;
	private String _server = null;
	private String _application = DEFAULT_APPLICATION;
	private String _eventMessageFile = "";
	private String _categoryMessageFile = "";

	private HANDLE _handle = null;

	public static Win32EventLogAppender createAppender(int messageID, String server, String source,
			String application, String eventMessageFile, String categoryMessageFile) {
		return new Win32EventLogAppender(messageID, server, source, application, eventMessageFile,
				categoryMessageFile);
	}

	public Win32EventLogAppender(int messageID, String server, String source, String application,
			String eventMessageFile, String categoryMessageFile) {

		this.messageID = messageID;
		if (eventMessageFile != null) {
			String pathExpanded = resolveEnvVars(eventMessageFile);

			Path p = Paths.get(pathExpanded);

			if (Files.exists(p)) {
				if (p.isAbsolute())
					setEventMessageFile(eventMessageFile);
				else
					setEventMessageFile(p.toAbsolutePath().toString());
			}

		}

		if (categoryMessageFile != null) {
			String pathExpanded = resolveEnvVars(categoryMessageFile);
			Path p = Paths.get(pathExpanded);
			if (Files.exists(p)) {
				if (p.isAbsolute())
					setCategoryMessageFile(categoryMessageFile);
				else
					setCategoryMessageFile(p.toAbsolutePath().toString());

			}
		}

		this._server = server;
		setSource(source);
		setApplication(application);
	}

	public void setSource(String source) {

		if (source == null || source.length() == 0) {
			source = DEFAULT_SOURCE;
		}

		_source = source.trim();
	}

	public String getSource() {
		return _source;
	}

	public void setApplication(String application) {

		if (application == null || application.length() == 0) {
			application = DEFAULT_APPLICATION;
		}

		_application = application.trim();
	}

	public String getApplication() {
		return _application;
	}

	public void close() {
		if (_handle != null) {
			// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-deregistereventsource
			if (!Advapi32.INSTANCE.DeregisterEventSource(_handle)) {
				throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
			}
			_handle = null;
		}
	}

	public void setEventMessageFile(String eventMessageFile) {
		_eventMessageFile = eventMessageFile.trim();
	}

	public String getEventMessageFile() {
		return _eventMessageFile;
	}

	public void setCategoryMessageFile(String categoryMessageFile) {
		_categoryMessageFile = categoryMessageFile.trim();
	}

	public String getCategoryMessageFile() {
		return _categoryMessageFile;
	}

	private void registerEventSource() {
		close();

		try {
			_handle = registerEventSource(_server, _source, _application, _eventMessageFile, _categoryMessageFile);
		} catch (Exception e) {
			close();
			throw new RuntimeException("Could not register event source: " + e.getMessage(), e);
		}
	}

	public void activateOptions() {
		registerEventSource();
	}

	public void append(String message, int eventLogType, int category) {
		if (_handle == null) {
			registerEventSource();
		}

		String[] buffer = { message };

		// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-reporteventa
		// NOTE the age: Minimum supported client Windows 2000 Professional
		// Minimum supported server Windows 2000 Server
		// https://www.pinvoke.net/search.aspx?search=ReportEvent
		// https://github.com/java-native-access/jna/blob/master/contrib/platform/src/com/sun/jna/platform/win32/Advapi32.java#L1650

		if (Advapi32.INSTANCE.ReportEvent(_handle, eventLogType, category, messageID, null, buffer.length, 0, buffer,
				null) == false) {
			Exception e = new Win32Exception(Kernel32.INSTANCE.GetLastError());
			System.err.println("Failed to report event [" + message + "]." + e.toString());
		}

	}

	public void append(String message) {
		append(message, WinNT.EVENTLOG_INFORMATION_TYPE, 3);
	}

	public void finalize() {
		close();
	}

	private HANDLE registerEventSource(String server, String source, String application, String eventMessageFile,
			String categoryMessageFile) {
		String applicationKeyPath = EVENT_LOG_PATH + application;
		String eventSourceKeyPath = applicationKeyPath + "\\" + source;
		// https://github.com/java-native-access/jna/blob/master/contrib/platform/src/com/sun/jna/platform/win32/Advapi32Util.java#L623
		if (Advapi32Util.registryKeyExists(WinReg.HKEY_LOCAL_MACHINE, applicationKeyPath)) {
			if (Advapi32Util.registryKeyExists(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath)) {
				setVariableKeys(eventMessageFile, categoryMessageFile, eventSourceKeyPath);
			} else {
				createAndSetAllKeys(eventMessageFile, categoryMessageFile, eventSourceKeyPath);
			}
		} else {
			createAndSetAllKeys(eventMessageFile, categoryMessageFile, eventSourceKeyPath);
		}

		// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-deregistereventsource
		HANDLE h = Advapi32.INSTANCE.RegisterEventSource(server, source);
		if (h == null) {
			throw new Win32Exception(Kernel32.INSTANCE.GetLastError());
		}

		return h;
	}

	private void createAndSetAllKeys(String eventMessageFile, String categoryMessageFile, String eventSourceKeyPath) {
		if (Advapi32Util.registryCreateKey(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath)) {
			Advapi32Util.registrySetIntValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, "TypesSupported",
					TYPES_SUPPORTED);
			Advapi32Util.registrySetIntValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, "CategoryCount",
					CATEGORY_COUNT);
			setVariableKeys(eventMessageFile, categoryMessageFile, eventSourceKeyPath);
		}
	}

	private void setVariableKeys(String eventMessageFile, String categoryMessageFile, String eventSourceKeyPath) {
		// https://github.com/java-native-access/jna/blob/master/contrib/platform/src/com/sun/jna/platform/win32/Advapi32Util.java#L667
		// NOTE: "Application" has CATEGORY_MESSAGE_FILE but no
		// EVENT_MESSAGE_FILE
		if (!Advapi32Util.registryValueExists(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, EVENT_MESSAGE_FILE)
				|| !Advapi32Util
						.registryGetStringValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, EVENT_MESSAGE_FILE)
						.equalsIgnoreCase(eventMessageFile)) {
			// skip creating one
			// Advapi32Util.registrySetStringValue(WinReg.HKEY_LOCAL_MACHINE,
			// eventSourceKeyPath, EVENT_MESSAGE_FILE, eventMessageFile);
		}
		if (!Advapi32Util.registryValueExists(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, CATEGORY_MESSAGE_FILE)
				|| !Advapi32Util
						.registryGetStringValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, CATEGORY_MESSAGE_FILE)
						.equalsIgnoreCase(categoryMessageFile)) {

			Advapi32Util.registrySetStringValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, CATEGORY_MESSAGE_FILE,
					categoryMessageFile);
		}
	}

	private static String resolveEnvVars(String input) {
		if (null == input) {
			return null;
		}
		Pattern p = Pattern.compile("%(\\w+)%");
		Matcher m = p.matcher(input);
		StringBuffer sb = new StringBuffer();
		while (m.find()) {
			String envVarName = m.group(1);
			String envVarValue = System.getenv(envVarName);
			m.appendReplacement(sb, null == envVarValue ? "" : envVarValue.replace("\\", "\\\\"));
		}
		m.appendTail(sb);
		return sb.toString();
	}

}
