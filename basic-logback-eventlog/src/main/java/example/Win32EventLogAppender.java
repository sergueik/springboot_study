package example;

import java.io.Serializable;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;

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

	private String _source = null;
	private String _server = null;
	private String _application = DEFAULT_APPLICATION;
	private String _eventMessageFile = "";
	private String _categoryMessageFile = "";

	private HANDLE _handle = null;

	public static Win32EventLogAppender createAppender(String name, String server, String source, String application,
			String eventMessageFile, String categoryMessageFile) {
		return new Win32EventLogAppender(name, server, source, application, eventMessageFile, categoryMessageFile);
	}

	public Win32EventLogAppender(String name, String server, String source, String application, String eventMessageFile,
			String categoryMessageFile) {

		if (eventMessageFile != null) {
			Path p = Paths.get(eventMessageFile);
			if (Files.exists(p)) {
				setEventMessageFile(p.toAbsolutePath().toString());
			}
		}

		if (categoryMessageFile != null) {
			Path p = Paths.get(categoryMessageFile);
			if (Files.exists(p)) {
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
			throw new RuntimeException("Could not register event source.", e);
		}
	}

	public void activateOptions() {
		registerEventSource();
	}

	public void append(String message, int eventLogType, int category) {
		if (_handle == null) {
			registerEventSource();
		}

		final int messageID = 0x1000;

		String[] buffer = { message };
		// https://learn.microsoft.com/en-us/windows/win32/api/winbase/nf-winbase-reporteventa
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
		if (!Advapi32Util.registryValueExists(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, EVENT_MESSAGE_FILE)
				|| !Advapi32Util
						.registryGetStringValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, EVENT_MESSAGE_FILE)
						.equalsIgnoreCase(eventMessageFile)) {
			Advapi32Util.registrySetStringValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, EVENT_MESSAGE_FILE,
					eventMessageFile);
		}
		if (!Advapi32Util.registryValueExists(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, CATEGORY_MESSAGE_FILE)
				|| !Advapi32Util
						.registryGetStringValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, CATEGORY_MESSAGE_FILE)
						.equalsIgnoreCase(categoryMessageFile)) {
			Advapi32Util.registrySetStringValue(WinReg.HKEY_LOCAL_MACHINE, eventSourceKeyPath, CATEGORY_MESSAGE_FILE,
					categoryMessageFile);
		}
	}

}
