
package example;

// origin:
// http://www.java2s.com/Code/Java/Threads/JobScheduler.htm
// Copyright (c) 1997-1999 Scott Oaks and Henry Wong.

import java.util.*;
import example.DaemonLock;
import example.ThreadPool;

public class JobScheduler implements Runnable {

	private final static int interval = 1000;

	public static void main(String[] args) throws Exception {
		Runnable r1 = new Runnable() {
			public void run() {
				System.out.print("1");
				try {
					Thread.sleep(interval);
				} catch (Exception ex) {
				}
				System.out.print("1");
			}
		};
		Runnable r2 = new Runnable() {
			public void run() {
				System.out.print("2");
				try {
					Thread.sleep(interval);
				} catch (Exception ex) {
				}
				System.out.print("2");
			}
		};
		Runnable r3 = new Runnable() {
			public void run() {
				System.out.print("3");
			}
		};
		Runnable r4 = new Runnable() {
			public void run() {
				System.out.print("4");
			}
		};

		JobScheduler js = new JobScheduler(0);
		Thread.sleep(1000);

		for (int cnt = 0; cnt != 10; cnt++) {
			Thread.sleep(1000);
			js.execute(r1);
		}
		// Test 1 - General Test
		// System.err.println("General Test: execute and repeat running two jobs");
		// js.executeInAndRepeat(r1, 10000, 3000, 10);
		// js.executeInAndRepeat(r2, 20000, 1000, 10);

		// Available functionality
		// Thread.sleep(11000);
		// js.cancel(r1);
		// js.cancel(r2);

		// js.configureBackup(r1);
		// Test 2 - Signature Test
		// Date in10Sec = new Date(System.currentTimeMillis()+10000L);
		// js.execute(r1);
		// js.executeIn(r2, 2000L);
		// js.executeInAndRepeat(r3, 10000L, 2000L);
		// js.executeInAndRepeat(r4, 10000L, 2000L, 5);
		// js.executeAt(r1, in10Sec);
		// js.executeAtAndRepeat(r2, in10Sec, 2000L);
		// js.executeAtAndRepeat(r3, in10Sec, 1000L, 5);
		// js.cancel(r4);
		// Thread.sleep(20000L);
		// js.cancel(r2);

		// Test 3 - Interval Test
		// js.executeInAndRepeat(r3, 10000L, JobScheduler.HOURLY);
		// js.executeInAndRepeat(r3, 10000L, JobScheduler.DAILY);
		// js.executeInAndRepeat(r3, 10000L, JobScheduler.WEEKLY);
		// js.executeInAndRepeat(r3, 10000L, JobScheduler.MONTHLY);
		// js.executeInAndRepeat(r3, 10000L, JobScheduler.YEARLY);
	}

	final public static int ONCE = 1;
	final public static int FOREVER = -1;
	final public static long HOURLY = (long) 60 * 60 * 1000;
	final public static long DAILY = 24 * HOURLY;
	final public static long WEEKLY = 7 * DAILY;
	final public static long MONTHLY = -1;
	final public static long YEARLY = -2;

	private class JobNode {
		public Runnable job;
		public Date executeAt;
		public long interval;
		public int count;
	}

	private ThreadPool tp;
	private DaemonLock dlock = new DaemonLock();
	private Vector<JobNode> jobs = new Vector<>(100);

	public JobScheduler(int poolSize) {
		tp = (poolSize > 0) ? new ThreadPool(poolSize) : null;
		Thread js = new Thread(this);
		js.setDaemon(true);
		js.start();
	}

	private synchronized void addJob(JobNode job) {
		dlock.acquire();
		jobs.addElement(job);
		notify();
	}

	private synchronized void deleteJob(Runnable job) {
		for (int i = 0; i < jobs.size(); i++) {
			// removed redundant cast to JobNode
			if ((jobs.elementAt(i)).job == job) {
				jobs.removeElementAt(i);
				dlock.release();
				break;
			}
		}
	}

	private JobNode updateJobNode(JobNode jn) {
		Calendar cal = Calendar.getInstance();
		cal.setTime(jn.executeAt);
		if (jn.interval == MONTHLY) {
			// There is a minor bug. (see java.util.calendar)
			cal.add(Calendar.MONTH, 1);
			jn.executeAt = cal.getTime();
		} else if (jn.interval == YEARLY) {
			cal.add(Calendar.YEAR, 1);
			jn.executeAt = cal.getTime();
		} else {
			jn.executeAt = new Date(jn.executeAt.getTime() + jn.interval);
		}
		jn.count = (jn.count == FOREVER) ? FOREVER : jn.count - 1;
		return (jn.count != 0) ? jn : null;
	}

	private synchronized long runJobs() {
		long minDiff = Long.MAX_VALUE;
		long now = System.currentTimeMillis();

		for (int i = 0; i < jobs.size();) {
			// removed redundant cast to JobNode
			JobNode jn = jobs.elementAt(i);
			if (jn.executeAt.getTime() <= now) {
				if (tp != null) {
					tp.addRequest(jn.job);
				} else {
					Thread jt = new Thread(jn.job);
					jt.setDaemon(false);
					jt.start();
				}
				if (updateJobNode(jn) == null) {
					jobs.removeElementAt(i);
					dlock.release();
				}
			} else {
				long diff = jn.executeAt.getTime() - now;
				minDiff = Math.min(diff, minDiff);
				i++;
			}
		}
		return minDiff;
	}

	public synchronized void run() {
		while (true) {
			long waitTime = runJobs();
			try {
				wait(waitTime);
			} catch (Exception e) {
			}
			;
		}
	}

	public void execute(Runnable job) {
		executeIn(job, (long) 0);
	}

	public void executeIn(Runnable job, long millis) {
		executeInAndRepeat(job, millis, 1000, ONCE);

	}

	public void executeInAndRepeat(Runnable job, long millis, long repeat) {
		executeInAndRepeat(job, millis, repeat, FOREVER);

	}

	public void executeInAndRepeat(Runnable job, long millis, long repeat,
			int count) {
		Date when = new Date(System.currentTimeMillis() + millis);
		executeAtAndRepeat(job, when, repeat, count);
	}

	public void executeAt(Runnable job, Date when) {
		executeAtAndRepeat(job, when, 1000, ONCE);
	}

	public void executeAtAndRepeat(Runnable job, Date when, long repeat) {
		executeAtAndRepeat(job, when, repeat, FOREVER);
	}

	public void executeAtAndRepeat(Runnable job, Date when, long repeat,
			int count) {
		JobNode jn = new JobNode();
		jn.job = job;
		jn.executeAt = when;
		jn.interval = repeat;
		jn.count = count;
		addJob(jn);
	}

	public void cancel(Runnable job) {
		deleteJob(job);
	}

	public void executeAtNextDOW(Runnable job, Date when, int DOW) {
		Calendar target = Calendar.getInstance();
		target.setTime(when);
		while (target.get(Calendar.DAY_OF_WEEK) != DOW)
			target.add(Calendar.DATE, 1);
		executeAt(job, target.getTime());
	}

	public void configureBackup(Runnable job) {
		Calendar now = Calendar.getInstance();

		executeAtNextDOW(job, now.getTime(), Calendar.SUNDAY);
	}

}
