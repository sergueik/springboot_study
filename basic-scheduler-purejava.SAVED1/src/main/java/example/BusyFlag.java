package example;

// origin:
// http://www.java2s.com/Code/Java/Threads/BusyFlag.htm
// Copyright (c) 1997-1999 Scott Oaks and Henry Wong.

public class BusyFlag {
	protected Thread busyflag = null;
	protected int busycount = 0;

	public synchronized void getBusyFlag() {
		while (tryGetBusyFlag() == false) {
			try {
				wait();
			} catch (Exception e) {
			}
		}
	}

	public synchronized boolean tryGetBusyFlag() {
		if (busyflag == null) {
			busyflag = Thread.currentThread();
			busycount = 1;
			return true;
		}
		if (busyflag == Thread.currentThread()) {
			busycount++;
			return true;
		}
		return false;
	}

	public synchronized void freeBusyFlag() {
		if (getBusyFlagOwner() == Thread.currentThread()) {
			busycount--;
			if (busycount == 0) {
				busyflag = null;
				notify();
			}
		}
	}

	public synchronized Thread getBusyFlagOwner() {
		return busyflag;
	}
}
