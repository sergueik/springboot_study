package com.findinpath.unleash.strategy;

import no.finn.unleash.UnleashContext;
import no.finn.unleash.strategy.Strategy;

import java.util.Map;
import java.util.Optional;

import static java.util.Arrays.asList;

/**
 * Custom Unleash strategy used for enabling a feature
 * toggle only for specific device classes (extracted from the `User-Agent` request header).
 *
 * @see {@link nl.basjes.parse.useragent.classify.DeviceClass}
 */
public class DeviceClassStrategy implements Strategy {


    public static final String PARAM = "device";
    private static final String STRATEGY_NAME = "deviceClass";

    /**
     * The name for the strategy as configured on the Unleash server.
     *
     * @return the name for the strategy
     */
    @Override
    public String getName() {
        return STRATEGY_NAME;
    }

    @Override
    public boolean isEnabled(Map<String, String> parameters) {
        return false;
    }

    @Override
    public boolean isEnabled(Map<String, String> parameters, UnleashContext unleashContext) {
        var properties = unleashContext.getProperties();
        if (properties.containsKey(PARAM)) {
            String device = unleashContext.getProperties().get(PARAM);
            return Optional.ofNullable(device)
                    .map(currentDevice -> Optional.ofNullable(parameters.get(PARAM))
                            .map(devicesString -> asList(devicesString.split(",\\s?")))
                            .filter(f -> f.contains(currentDevice)).isPresent())
                    .orElse(false);

        }
        return false;
    }
}
