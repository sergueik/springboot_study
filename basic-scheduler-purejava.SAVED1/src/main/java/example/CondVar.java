package example;

// origin:
// http://www.java2s.com/Code/Java/Threads/BusyFlag.htm
// Copyright (c) 1997-1999 Scott Oaks and Henry Wong.

public class CondVar {
	private BusyFlag SyncVar;

	public CondVar() {
		this(new BusyFlag());
	}

	public CondVar(BusyFlag sv) {
		SyncVar = sv;
	}

	public void cvWait() throws InterruptedException {
		cvTimedWait(SyncVar, 0);
	}

	public void cvWait(BusyFlag sv) throws InterruptedException {
		cvTimedWait(sv, 0);
	}

	public void cvTimedWait(int millis) throws InterruptedException {
		cvTimedWait(SyncVar, millis);
	}

	public void cvTimedWait(BusyFlag sv, int millis) throws InterruptedException {
		int i = 0;
		InterruptedException errex = null;

		synchronized (this) {
			// You must own the lock in order to use this method
			if (sv.getBusyFlagOwner() != Thread.currentThread()) {
				throw new IllegalMonitorStateException("current thread not owner");
			}

			// Release the lock (Completely)
			while (sv.getBusyFlagOwner() == Thread.currentThread()) {
				i++;
				sv.freeBusyFlag();
			}

			// Use wait() method
			try {
				if (millis == 0) {
					wait();
				} else {
					wait(millis);
				}
			} catch (InterruptedException iex) {
				errex = iex;
			}
		}

		// Obtain the lock (Return to original state)
		for (; i > 0; i--) {
			sv.getBusyFlag();
		}

		if (errex != null)
			throw errex;
		return;
	}

	public void cvSignal() {
		cvSignal(SyncVar);
	}

	public synchronized void cvSignal(BusyFlag sv) {
		// You must own the lock in order to use this method
		if (sv.getBusyFlagOwner() != Thread.currentThread()) {
			throw new IllegalMonitorStateException("current thread not owner");
		}
		notify();
	}

	public void cvBroadcast() {
		cvBroadcast(SyncVar);
	}

	public synchronized void cvBroadcast(BusyFlag sv) {
		// You must own the lock in order to use this method
		if (sv.getBusyFlagOwner() != Thread.currentThread()) {
			throw new IllegalMonitorStateException("current thread not owner");
		}
		notifyAll();
	}
}
