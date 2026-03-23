package io.github.kaluchi.jdtbridge;

import org.eclipse.ui.IStartup;

/**
 * Registered via org.eclipse.ui.startup extension point.
 * Forces early activation of the bundle so the Activator runs at Eclipse startup.
 */
public class StartupHandler implements IStartup {

    @Override
    public void earlyStartup() {
        // Intentionally empty — bundle activation is enough.
        // The Activator.start() already starts the HTTP server.
    }
}
