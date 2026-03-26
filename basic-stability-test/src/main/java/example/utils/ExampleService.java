package example.utils;

import java.io.File;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Copyright 2026 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Autowired;

import org.springframework.stereotype.Component;
import org.springframework.stereotype.Service;

@Service
// NOTE: one should never name the class 'Service': collision with stereotype:
// incompatible types: example.Service cannot be converted to
// java.lang.annotation.Annotation

public class ExampleService {
	private static final Logger log = LoggerFactory.getLogger(ExampleService.class);

	// "Stability detection via successive sampling"
	// considered stable once
	// sequence of observations has converged:
	// successive samples become identical (or within tolerance)
	public boolean waitStable(final File file, int retries) {
		int stableCount = 0;
		int iteration = retries;
		long prevSize = 0L;
		long prevMtime = 0L;
		long interval = 100;
		int threshold = 2;
		while (retries-- > 0) {
			long size = file.length();
			long mtime = file.lastModified();
			log.info("iteration {} size: {} mtime {}", (iteration - retries), size, mtime);

			if (size == prevSize && mtime == prevMtime) {
				stableCount++;
			} else {
				stableCount = 0;
			}

			if (stableCount >= threshold) {
				return true;
			}

			prevSize = size;
			prevMtime = mtime;
			try {
				Thread.sleep(interval);
			} catch (InterruptedException e) {
				return false;
			}
		}
		return false;
	}

}
