/*
 * Copyright 2018-2019 midorlo, sergueik
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package example;

import static org.hamcrest.CoreMatchers.notNullValue;
import static org.hamcrest.MatcherAssert.assertThat;
// import static org.hamcrest.Matchers.equalTo;
import static org.hamcrest.Matchers.greaterThan;

import static org.testng.Assert.assertEquals;
import static org.testng.Assert.assertTrue;
import static org.testng.Assert.assertFalse;
import static org.testng.Assert.fail;

import java.lang.reflect.Method;

import java.util.regex.Matcher;
import java.util.regex.Pattern;

import org.testng.Assert;
import org.testng.annotations.AfterMethod;
import org.testng.annotations.BeforeMethod;
import org.testng.annotations.Test;

import com.sun.jna.Memory;
import com.sun.jna.Pointer;
import com.sun.jna.WString;
import com.sun.jna.platform.win32.WTypes.LPWSTR;
import com.sun.jna.platform.win32.WinDef.RECT;

/**
 * @author midorlo
 * @author sergueik
 */
public class AU3Test {

	private String title = null;
	private String text = "";
	private final String commandline = "c:\\Windows\\System32\\notepad.exe";
	private String workdir = "c:\\Windows\\Temp";

	private final int au3Success = Constants.AU3_SUCCESS;
	private final int au3Failure = Constants.AU3_FAILURE;
	AutoItX instance = null;

	@BeforeMethod
	public void beforeMethod(Method method) {
		instance = AutoItX.getInstance();
	}

	@AfterMethod
	public void afterMethod() {
		// cleanup of instance ?
	}

	@Test(enabled = true)
	public void testGetSingletonInstance() {
		System.err.println("Get Singleton Instances");
		AutoItX instance2 = AutoItX.getInstance();
		assertEquals(instance, instance2);
	}

	@Test(enabled = true)
	public void testInit() {
		System.err.println("Init");
		instance.AU3_Init();
	}

	@Test(enabled = false)
	public void testCloseOpenFileDialog() {
		System.err.println("Close File Dialog");
		title = "Open"; // will switch to "Save"
		text = "";
		int result = instance.AU3_WinClose(new WString(title), new WString(text));
		assertEquals(result, au3Success);
	}

	@Test(enabled = false)
	public void testCloseChromeBrowser() {
		System.err.println("Close Chrome Browser");
		title = "New Tab";
		assertEquals(instance.AU3_WinClose(new WString(title), new WString(text)), au3Success);
	}

	@Test(enabled = false)
	public void testCloseFirefoxBrowser() {
		System.err.println("Close Mozilla Firefox Browser");
		title = "Mozilla Firefox Start Page";
		assertEquals(instance.AU3_WinClose(new WString(title), new WString(text)), au3Success);
	}

	@Test(enabled = false)
	public void testProvidePathToOpenFile() {
		System.err.println("Provide Path to Open File");
		title = "Open";
		instance.AU3_WinWaitActive(new WString(title), new WString(text));
		instance.AU3_Send(new WString("D:\\AutoIT-commands\\TestingVideo.mp4"));
		instance.AU3_Send(new WString("\n")); // "{ENTER}"
		// close the file not found sub-dialog
		instance.AU3_WinClose(new WString(title), new WString(text));
		// close the file open dialog
		// TODO: confirm the status
		assertEquals(instance.AU3_WinClose(new WString(title), new WString(text)), au3Failure);
	}

	@Test(enabled = true)
	public void testProcessState() {
		System.err.println("Get, create, stop Process using AU3 API");

		String regex = "^.*\\\\";
		Pattern pattern = Pattern.compile(regex);
		Matcher matcher = pattern.matcher(commandline);
		String processName = matcher.replaceAll("");
		title = "Untitled - Notepad";

		// System.err.println(
		// "Commandline: " + commandline + " processname : " + processName);
		int result = instance.AU3_ProcessExists(new WString(processName));
		System.err.println("Existing process check: "
				+ (result == 0 ? "not running" : String.format("already running with pid %d", result)));

		if (result == 0) {
			System.err.println(String.format("Launching process %s with commandline %s", processName, commandline));
			int pid = instance.AU3_Run(new WString(commandline), new WString(workdir), Constants.SW_SHOW);
			System.err.println("Launched process pid: " + pid);
		}
		assertTrue(instance.AU3_ProcessExists(new WString(processName)) != Constants.AU3_FAILURE);
		instance.AU3_Sleep(1000);
		assertTrue(instance.AU3_WinExists(new WString(title), new WString("")) != Constants.AU3_FAILURE);
		System.err.println("Window exists. ");
		System.err.println("Closing window title: " + title);
		instance.AU3_WinClose(new WString(title), new WString(""));
		instance.AU3_Sleep(1000);
		if (instance.AU3_WinExists(new WString(title), new WString("")) == Constants.AU3_SUCCESS) {
			System.err.println("Killing window title: " + title);
			instance.AU3_WinKill(new WString(title), new WString(""));
		}
		assertFalse(instance.AU3_WinExists(new WString(title), new WString("")) == Constants.AU3_SUCCESS);
	}

	@Test(enabled = true)
	public void testClipboard() {
		System.err.println("Put and get data using AU3 Clipboard API");
		String dataPut = "example";
		int dataPutLength = dataPut.length();
		instance.AU3_ClipPut(new WString(dataPut));
		try {
			StringBuilder sb = new StringBuilder(dataPutLength);
			while (sb.length() < dataPutLength) {
				sb.append("*");
			}
			String dataGet = sb.toString();
			// TODO: explain
			int bufSize = dataGet.length() + 1;
			LPWSTR resultPtr = new LPWSTR(dataGet);
			instance.AU3_ClipGet(resultPtr, bufSize);
			dataGet = resultPtr.getValue().toString();
			dataGet.trim();
			assertThat(dataGet, notNullValue());
			System.err.println("Got data: " + dataGet);
			Assert.assertTrue(dataGet.equals(dataPut));
		} catch (Exception e) {
			// Corrupted stdin stream in forked JVM 1. Stream '#' - solved.
			System.err.println("Exception " + e.toString());
		}
	}

	@Test(enabled = true)
	public void testWinGetPos() {
		System.err.println("Get active window position information");
		title = "[ACTIVE]";
		assertTrue(instance.AU3_WinExists(new WString(title), new WString("")) != Constants.AU3_FAILURE);

		RECT rect = new RECT();
		instance.AU3_WinGetPos(new WString(title), new WString(""), rect);
		assertThat(rect, notNullValue());
		assertThat(rect.bottom, greaterThan(0));
		assertThat(rect.right, greaterThan(0));
		System.err.println(String.format("top: %d\nleft: %d\nbottom: %d\nright: %d", rect.top, rect.left, rect.bottom,
				rect.right));

	}

	@Test(enabled = true)
	public void testMouseGetPos() {
		System.err.println("Get mouse position information");
		try {

			int[] pos = new int[2];
			Pointer arg = new Memory(32);
			instance.AU3_MouseGetPos(arg);
			System.err.println("Dump: " + arg.dump(0, 16));
			pos = arg.getIntArray(0, 2);

			assertThat(pos, notNullValue());

			assertThat(pos[0], greaterThan(-1));
			assertThat(pos[1], greaterThan(-1));

			System.err.println(String.format("X: %d\nY: %d", pos[0], pos[1]));

		} catch (Exception e) {
			// Exception java.lang.IndexOutOfBoundsException: Bounds exceeds available space
			// : size=4, offset = 8
			System.err.println("Exception " + e.toString());
		}
	}

	@Test(enabled = true)
	public void testDesktopWindowDimensions() {
		System.err.println("Get desktop window position information");
		title = "Program Manager";

		RECT rect = new RECT();
		instance.AU3_WinGetPos(new WString(title), new WString(""), rect);
		assertThat(rect, notNullValue());
		assertEquals(rect.top, 0);
		assertEquals(rect.left, 0);
		assertThat(rect.bottom, greaterThan(0));
		assertThat(rect.right, greaterThan(0));
		System.err.println(String.format("top: %d\nleft: %d\nbottom: %d\nright: %d", rect.top, rect.left, rect.bottom,
				rect.right));

	}

	// @Test(enabled = false)
	// public void testAU3_WinActive() {
	// System.err.println("AU3_WinActive");
	// WString arg0 = null;
	// WString arg1 = null;
	// int result = instance.AU3_WinActive(arg0, arg1);
	// assertEquals(result, au3Success);
	// }

	// @Test(enabled = false)
	// public void testAU3_WinExists() {
	// System.err.println("AU3_WinExists");
	// WString arg0 = null;
	// WString arg1 = null;
	// int result = instance.AU3_WinExists(arg0, arg1);
	// assertEquals(result, au3Success);
	// }

	// @Test(enabled = false)
	// public void testAU3_WinGetTitle() {
	// System.err.println("AU3_WinGetTitle");
	// WString arg0 = null;
	// WString arg1 = null;
	// WTypes.LPWSTR arg2 = null;
	// int arg3 = 0;
	// instance.AU3_WinGetTitle(arg0, arg1, arg2, arg3);
	// }

}
