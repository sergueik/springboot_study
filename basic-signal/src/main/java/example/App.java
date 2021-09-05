package example;

import java.util.Observer;
import java.util.Observable;

// origin: https://ringlord.com/dl/Signals-in-Java.pdf
@SuppressWarnings("restriction")

public class App implements Observer {

	private final int delay = 240;
	private static String name = "USR2";
	// not working - not receiving USR2

	public static void main(final String[] args) {
		name = "SYS";
		new App().go();
	}

	private void go() {
		try {
			final CustomSignalHandler sh = new CustomSignalHandler();
			sh.addObserver(this);

			// sh.handleSignal("USR1");
			// Signal already used by VM or OS: SIGUSR1
			// Signal already used by VM or OS: SIGHUP
			// the documented way
			// https://www.ibm.com/docs/en/ztpf/2019?topic=signals-used-by-jvm
			// to change it via -Xrs flag does not appear to work
			// https://itsiti.com/list-of-signal-names-in-unix-linux/
			sh.handleSignal(name);
			System.err.println(String.format("Sleeping for %d seconds: hit me with signals!", delay));
			new Thread(new Runnable() {
				@Override
				public void run() {
					while (true) {
						try {
							Thread.sleep(1000 * delay);
						} catch (Exception e) {
							e.printStackTrace();
						}
					}
				}
			}).start();

			// Thread.sleep(1000 * delay);
		} catch (Throwable e) {
			e.printStackTrace();
		}

	}

	public void update(final Observable o, final Object arg) {
		System.err.println("Received notification of signal: " + arg);
	}

}
