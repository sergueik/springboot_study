// based on: https://crunchify.com/what-is-daemon-thread-in-java-example-attached/

public class DaemonExample extends Thread {
	private static final boolean bound = Boolean.parseBoolean(System.getenv("BOUND"));
	// false Worker thread continues to run.
	// true Worker thread terminates with the main thread
	private static final boolean debug = Boolean.parseBoolean(System.getenv("DEBUG"));

	public static void main(String[] args) {
		int delay;
		try {
			delay = Integer.parseInt(System.getenv("DELAY"));
		} catch (NumberFormatException e) {
			delay = 3000;
		}
		if (debug) {
			System.err.println("Main thread starts");
		}
		DaemonExample t = new DaemonExample();
		t.setDaemon(bound);
		t.start();

		try {
			Thread.sleep(delay);
		} catch (InterruptedException x) {
		}

		if (debug) {
			System.err.println("Main thread exit");
		}
	}

	public void run() {
		Boolean continueRun = true;
		try {
			while (continueRun) {
				continueRun = work();
			}
		} finally {
			System.out.println("background thread exits");
		}
	}

	private Boolean work() {
		try {
			Thread.sleep(1000);
		} catch (InterruptedException x) {
		}
		if (debug) {

			System.err.println("background thread # " + Thread.currentThread().getId() + " works");
		}
		return true;
	}
}
