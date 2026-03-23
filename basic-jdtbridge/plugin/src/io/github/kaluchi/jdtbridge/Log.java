package io.github.kaluchi.jdtbridge;

import org.eclipse.core.runtime.ILog;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.Platform;
import org.eclipse.core.runtime.Status;

/**
 * Centralized logging via Eclipse ILog.
 * Messages appear in Error Log view and {@code <workspace>/.metadata/.log}.
 */
class Log {

    private static final String BUNDLE_ID =
            "io.github.kaluchi.jdtbridge";
    private static final ILog LOGGER =
            Platform.getLog(Log.class);

    static void info(String msg) {
        LOGGER.log(new Status(IStatus.INFO, BUNDLE_ID, msg));
    }

    static void warn(String msg) {
        LOGGER.log(new Status(IStatus.WARNING, BUNDLE_ID, msg));
    }

    static void warn(String msg, Throwable t) {
        LOGGER.log(new Status(
                IStatus.WARNING, BUNDLE_ID, msg, t));
    }

    static void error(String msg) {
        LOGGER.log(new Status(IStatus.ERROR, BUNDLE_ID, msg));
    }

    static void error(String msg, Throwable t) {
        LOGGER.log(new Status(
                IStatus.ERROR, BUNDLE_ID, msg, t));
    }
}
