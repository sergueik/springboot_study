package example.custom;
/**
 * Copyright 2021, 2022 Serguei Kouzmine
 */

import java.io.PrintStream;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

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
	private boolean debug = false;
	private boolean verbose = false;
	private static int size = 120; // 5 min is too long

	public void setDebug(boolean value) {
		debug = value;
	}

	public void setVerbose(boolean value) {
		verbose = value;
	}

	private static final Pdh pdh = Pdh.INSTANCE;
	private static List<DataEntry> list = Collections
			.synchronizedList(new CircularList(size));
	private MessageType task = MessageType.COLLECT;

	public void setTask(MessageType task) {
		this.task = task;
	}

	@Override
	public void run() {

		if (task.equals(MessageType.COLLECT)) {
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
					CounterData counterData = collectCounterData(counterName, rawCounter);
					if (verbose) {
						showRawCounterData(counterData, System.out);
					}
				} finally {
					assertErrorSuccess("PdhRemoveCounter",
							pdh.PdhRemoveCounter(hCounter));
				}
			} finally {
				assertErrorSuccess("PdhCloseQuery", pdh.PdhCloseQuery(hQuery));
			}
		} else {
			ComputedResult computedResult = computeAverage();
			if (verbose) {
				showAverage(computedResult, System.out);
			}
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

	// http://www.java2s.com/example/java/java.util/add-minutes-to-date.html
	public static Date minutesBeforeDate(int minutes, Date beforeTime) {
		final long ONE_MINUTE_IN_MILLIS = 60000;
		long curTimeInMs = beforeTime.getTime();
		Date afterAddingMins = new Date(
				curTimeInMs - (minutes * ONE_MINUTE_IN_MILLIS));
		return afterAddingMins;
	}

	private ComputedResult computeAverage() {
		int minutes = 1;
		return computeAverage(minutes);
	}

	private ComputedResult computeAverage(int minutes) {
		ComputedResult result = new ComputedResult();
		synchronized (list) {
			int cnt = list.size();
			if (cnt == 0) {
				cnt = PerformanceCounterTask.size;
			}
			if (debug) {
				System.err.println(String.format("averaging of %d values", cnt));
			}
			Date currentDate = new Date();
			Date checkDate = minutesBeforeDate(minutes, currentDate);

			List<DataEntry> dataList = Arrays.asList((DataEntry[]) list.toArray());

			if (debug) {
				// NOTE: safety against NPE in the first run
				dataList.stream().filter(o -> o != null)
						.filter(o -> o.getDate().after(checkDate)).map(o -> String
								.format("%s %d\n", o.getTimestampString(), o.getValue()))
						.forEach(o -> System.err.println(o));
			}

			// https://docs.oracle.com/javase/tutorial/collections/streams/index.html
			// NOTE: The method average() is undefined for the type Stream<Long>
			// NOTE: safety against NPE in the first run
			double averageValue = dataList.stream().filter(o -> o != null)
					.filter(o -> o.getDate().after(checkDate))
					.mapToLong(DataEntry::getValue).average().getAsDouble();

			// http://www.java2s.com/Tutorials/Java/Java_Stream/0250__Java_Stream_Count.htm
			// NOTE: safety against NPE in the first run
			long entryCount = dataList.stream().filter(o -> o != null)
					.filter(o -> o.getDate().after(checkDate)).count();

			result.minutes = minutes;
			result.entryCount = entryCount;
			result.size = cnt;
			result.averageValue = averageValue;
		}
		return result;
	}

	private void showAverage(ComputedResult result, PrintStream out) {
		// double averageValue = result.averageValue;
		// long entryCount = result.entryCount;
		// int size = result.size;
		// int minutes = result.minutes;
		out.append(String.format("%d %,2f (%d/%d)", result.minutes,
				result.averageValue, result.entryCount, result.size)).println();

	}

	private CounterData collectCounterData(String counterName,
			PDH_RAW_COUNTER rawCounter) {
		CounterData counterData = new CounterData();

		counterData.firstValue = rawCounter.FirstValue;
		counterData.counterName = counterName;
		counterData.secondValue = rawCounter.SecondValue;
		counterData.multiCount = rawCounter.MultiCount;
		int cnt = 0;
		synchronized (list) {
			list.add(new DataEntry(rawCounter.FirstValue));
			cnt = list.size();
		}
		counterData.cnt = cnt;

		return counterData;
	}

	private static void showRawCounterData(CounterData counterData,
			PrintStream out) {
		String counterName = counterData.counterName;
		long firstValue = counterData.firstValue;
		long secondValue = counterData.secondValue;
		int multiCount = counterData.multiCount;
		int cnt = counterData.cnt;
		out.append('\t').append(" # ").append(String.valueOf(cnt))
				.append(counterName).append(" ").append(counterName).append(" 1st=")
				.append(String.valueOf(firstValue)).append(" 2nd=")
				.append(String.valueOf(secondValue)).append(" multi=")
				.append(String.valueOf(multiCount)).println();
	}
}
