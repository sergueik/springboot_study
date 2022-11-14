
package example;

/**
 * A fake transfer service simulating an actual one.
 */
public abstract class TransferService {

	public boolean transfer(long amount) {
		beforeTransfer(amount);
		// exchange messages with a remote system to transfer the money
		try {
			// simulate a varying transfer processing timeout in actual system
			Thread.sleep((long) (500 + Math.random() * 500));
		} catch (InterruptedException e) {
			// should never happen
		}
		// simulate both failing and successful transfers
		boolean outcome = Math.random() >= 0.25;
		afterTransfer(amount, outcome);
		return outcome;
	}

	abstract protected void beforeTransfer(long amount);

	abstract protected void afterTransfer(long amount, boolean outcome);
}
