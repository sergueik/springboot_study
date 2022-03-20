package example;

import java.io.PrintStream;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.Pdh;
import com.sun.jna.platform.win32.Pdh.PDH_COUNTER_PATH_ELEMENTS;
import com.sun.jna.platform.win32.Pdh.PDH_RAW_COUNTER;
import com.sun.jna.platform.win32.PdhMsg;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.DWORDByReference;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;

public class PerformanceCounterTask implements Runnable {
	private static final Pdh pdh = Pdh.INSTANCE;

	@Override
	public void run() {
		// TODO Auto-generated method stub
		PDH_COUNTER_PATH_ELEMENTS elems = new PDH_COUNTER_PATH_ELEMENTS();

		elems.szObjectName = "System";
		elems.szInstanceName = null;
		elems.szCounterName = "Processor Queue Length";
		String counterName = makeCounterPath(pdh, elems);

		HANDLEByReference ref = new HANDLEByReference();
		assertErrorSuccess("PdhOpenQuery", pdh.PdhOpenQuery(null, null, ref));

		HANDLE hQuery = ref.getValue();
		try {
			ref.setValue(null);
			assertErrorSuccess("PdhAddEnglishCounter",
					pdh.PdhAddEnglishCounter(hQuery, counterName, null, ref));

			HANDLE hCounter = ref.getValue();
			try {
				assertErrorSuccess("PdhCollectQueryData",
						pdh.PdhCollectQueryData(hQuery));

				DWORDByReference lpdwType = new DWORDByReference();
				PDH_RAW_COUNTER rawCounter = new PDH_RAW_COUNTER();
				assertErrorSuccess("PdhGetRawCounterValue",
						pdh.PdhGetRawCounterValue(hCounter, lpdwType, rawCounter));
				assertErrorSuccess("Counter data status", rawCounter.CStatus);
				showRawCounterData(System.out, counterName, rawCounter);
			} finally {
				assertErrorSuccess("PdhRemoveCounter", pdh.PdhRemoveCounter(hCounter));
			}
		} finally {
			assertErrorSuccess("PdhCloseQuery", pdh.PdhCloseQuery(hQuery));
		}

	}

	private static String makeCounterPath(Pdh pdh,
			PDH_COUNTER_PATH_ELEMENTS pathElements) {
		DWORDByReference pcchBufferSize = new DWORDByReference();
		int status = pdh.PdhMakeCounterPath(pathElements, null, pcchBufferSize, 0);
		if (status != PdhMsg.PDH_MORE_DATA) {
			throw new RuntimeException(
					"Unexpected status code: 0x" + Integer.toHexString(status));
		}
		DWORD bufSize = pcchBufferSize.getValue();
		int numChars = bufSize.intValue();
		if (numChars <= 0) {
			throw new RuntimeException("Bad required buffer size: " + numChars);
		}
		char[] szFullPathBuffer = new char[numChars + 1 /* the \0 */];
		pcchBufferSize.setValue(new DWORD(szFullPathBuffer.length));
		assertErrorSuccess("PdhMakeCounterPath", pdh
				.PdhMakeCounterPath(pathElements, szFullPathBuffer, pcchBufferSize, 0));

		return Native.toString(szFullPathBuffer);
	}

	private static final void assertErrorSuccess(String message, int statusCode) {
		if (statusCode != WinError.ERROR_SUCCESS) {
			throw new RuntimeException(
					message + " - failed - hr=0x" + Integer.toHexString(statusCode));
		}
	}

	private static void showRawCounterData(PrintStream out, String counterName,
			PDH_RAW_COUNTER rawCounter) {
		out.append('\t').append(counterName).append(" ")
				.append(String.valueOf(rawCounter.TimeStamp.toDate())).append(" 1st=")
				.append(String.valueOf(rawCounter.FirstValue)).append(" 2nd=")
				.append(String.valueOf(rawCounter.SecondValue)).append(" multi=")
				.append(String.valueOf(rawCounter.MultiCount)).println();
	}

}
