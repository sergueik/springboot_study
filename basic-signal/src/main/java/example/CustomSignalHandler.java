package example;

import sun.misc.SignalHandler;
import sun.misc.Signal;
import java.util.Observable;

@SuppressWarnings("restriction")
class CustomSignalHandler extends Observable implements SignalHandler {
	public void handleSignal(final String signalName) throws IllegalArgumentException {
		try {
			Signal signal = new Signal(signalName);
			System.err.println(
					String.format("CustomSignalHandler set to handle %s (%d)", signalName, signal.getNumber()));
			Signal.handle(signal, this);

			Signal.handle(signal, o -> {
				System.out.println("CustomSignalHandler handling signal " + o.getName() + " (" + o.getNumber() + ")");
			});
		} catch (IllegalArgumentException e) {
			System.err.println("Unexpected Exception: " + e.toString());
			throw e;
		} catch (Throwable e) {
			throw new IllegalArgumentException("Signal unsupported: " + signalName, e);
		}
	}

	public void handle(final Signal signal) {
		System.err.println(String.format("handling signal %s (%d)", signal.getName(), signal.getNumber()));
		setChanged();
		notifyObservers(signal);
	}
}