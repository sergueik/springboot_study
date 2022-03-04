package example;

// origin:
// http://www.java2s.com/Code/Java/Threads/ThreadPool2.htm
// Copyright (c) 1997-1999 Scott Oaks and Henry Wong.

import java.util.*;


import example.CondVar;
import example.BusyFlag;


public class ThreadPool {

	class ThreadPoolRequest {
		Runnable target;
		Object lock;

		ThreadPoolRequest(Runnable t, Object l) {
			target = t;
			lock = l;
		}
	}

	class ThreadPoolThread extends Thread {
		ThreadPool parent;
		boolean shouldRun = true;

		ThreadPoolThread(ThreadPool parent, int i) {
			super("ThreadPoolThread " + i);
			this.parent = parent;
		}

		public void run() {
			ThreadPoolRequest obj = null;
			while (shouldRun) {
				try {
					parent.cvFlag.getBusyFlag();
					while (obj == null && shouldRun) {
						try {
							// removed redundant cast to ThreadPoolRequest
							obj = parent.objects.elementAt(0);
							parent.objects.removeElementAt(0);
						} catch (ArrayIndexOutOfBoundsException aiobe) {
							obj = null;
						} catch (ClassCastException cce) {
							System.err.println("Unexpected data");
							obj = null;
						}
						if (obj == null) {
							try {
								parent.cvAvailable.cvWait();
							} catch (InterruptedException ie) {
								return;
							}
						}
					}
				} finally {
					parent.cvFlag.freeBusyFlag();
				}
				if (!shouldRun)
					return;
				obj.target.run();
				try {
					parent.cvFlag.getBusyFlag();
					nObjects--;
					if (nObjects == 0)
						parent.cvEmpty.cvSignal();
				} finally {
					parent.cvFlag.freeBusyFlag();
				}
				if (obj.lock != null) {
					synchronized (obj.lock) {
						obj.lock.notify();
					}
				}
				obj = null;
			}
		}
	}

	Vector<ThreadPoolRequest> objects;
	int nObjects = 0;
	CondVar cvAvailable, cvEmpty;
	BusyFlag cvFlag;
	ThreadPoolThread poolThreads[];
	boolean terminated = false;

	public ThreadPool(int n) {
		cvFlag = new BusyFlag();
		cvAvailable = new CondVar(cvFlag);
		cvEmpty = new CondVar(cvFlag);
		objects = new Vector<ThreadPoolRequest>();
		poolThreads = new ThreadPoolThread[n];
		for (int i = 0; i < n; i++) {
			poolThreads[i] = new ThreadPoolThread(this, i);
			poolThreads[i].start();
		}
	}

	private void add(Runnable target, Object lock) {
		try {
			cvFlag.getBusyFlag();
			if (terminated)
				throw new IllegalStateException("Thread pool has shutdown");
			objects.addElement(new ThreadPoolRequest(target, lock));
			nObjects++;
			cvAvailable.cvSignal();
		} finally {
			cvFlag.freeBusyFlag();
		}
	}

	public void addRequest(Runnable target) {
		add(target, null);
	}

	public void addRequestAndWait(Runnable target) throws InterruptedException {
		Object lock = new Object();
		synchronized (lock) {
			add(target, lock);
			lock.wait();
		}
	}

	public void waitForAll(boolean terminate) throws InterruptedException {
		try {
			cvFlag.getBusyFlag();
			while (nObjects != 0)
				cvEmpty.cvWait();
			if (terminate) {
				for (int i = 0; i < poolThreads.length; i++)
					poolThreads[i].shouldRun = false;
				cvAvailable.cvBroadcast();
				terminated = true;
			}
		} finally {
			cvFlag.freeBusyFlag();
		}
	}

	public void waitForAll() throws InterruptedException {
		waitForAll(false);
	}
}
