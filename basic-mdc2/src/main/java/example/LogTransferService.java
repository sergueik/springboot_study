package example;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import example.TransferService;

final class LogTransferService extends TransferService {

	private static final Logger logger = LoggerFactory
			.getLogger(LogTransferService.class);

	@Override
	protected void beforeTransfer(long amount) {
		logger.info("Preparing to transfer {}$.", amount);
	}

	@Override
	protected void afterTransfer(long amount, boolean outcome) {
		logger.info("Has transfer of {}$ completed successfully ? {}.", amount,
				outcome);
	}

}
