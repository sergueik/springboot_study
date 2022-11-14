package example;

import org.slf4j.MDC;

import example.Transfer;

public class LogRunnable implements Runnable {
	private final Transfer tx;

	public LogRunnable(Transfer tx) {
		this.tx = tx;
	}

	public void run() {

		MDC.put("transaction.id", tx.getTransactionId());
		MDC.put("transaction.owner", tx.getSender());

		new LogTransferService().transfer(tx.getAmount());

		MDC.clear();
		// NOTE:
		// don't need this with MdcAwareThreadPoolExecutor
		// which is somewhat advanced

	}
}
