package example;

/**
 * Copyright 2021, 2022 Serguei Kouzmine
 */

import java.util.*;
import example.DaemonLock;
import example.ThreadPool;

import java.io.PrintStream;

import org.junit.Ignore;
import org.junit.Test;

import com.sun.jna.Native;
import com.sun.jna.platform.win32.Pdh;
import com.sun.jna.platform.win32.PdhMsg;
import com.sun.jna.platform.win32.WinError;
import com.sun.jna.platform.win32.Pdh.PDH_COUNTER_PATH_ELEMENTS;
import com.sun.jna.platform.win32.Pdh.PDH_RAW_COUNTER;
import com.sun.jna.platform.win32.PdhUtil.PdhEnumObjectItems;
import com.sun.jna.platform.win32.PdhUtil.PdhException;
import com.sun.jna.platform.win32.WinDef.DWORD;
import com.sun.jna.platform.win32.WinDef.DWORDByReference;
import com.sun.jna.platform.win32.WinNT.HANDLE;
import com.sun.jna.platform.win32.WinNT.HANDLEByReference;
import org.junit.Assert;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.fail;

public class PerformanceCounterJobScheduler {

	private final static int interval = 1000;

	public static void main(String[] args) throws Exception {

		Runnable collectorTask = new PerformanceCounterTask();
		PerformanceCounterTask computeTask = new PerformanceCounterTask();
		computeTask.setTask(MessageType.COMPUTE);
		JobScheduler jobScheduler = new JobScheduler(0);
		jobScheduler.executeInAndRepeat(collectorTask, interval,
				JobScheduler.PER_SECOND);
		jobScheduler.executeInAndRepeat(computeTask, interval * 10,
				30 * JobScheduler.PER_SECOND);
	}

}
