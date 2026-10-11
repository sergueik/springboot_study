package com.findinpath.unleash.config;

import com.findinpath.unleash.context.UnleashContextProvider;
import com.findinpath.unleash.strategy.DeviceClassStrategy;
import nl.basjes.parse.useragent.UserAgentAnalyzer;
import no.finn.unleash.DefaultUnleash;
import no.finn.unleash.Unleash;
import no.finn.unleash.util.UnleashConfig;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.net.InetAddress;
import java.net.UnknownHostException;

@Configuration
public class UnleashConfiguration {

    @Bean
    public UnleashConfig unleashConfig(
            @Value("${unleash.appName}") String appName,
            @Value("${unleash.endpoint}") String endpoint,
            @Value("${unleash.fetchTogglesInterval:60}") long fetchTogglesInterval,
            @Value("${unleash.sendMetricsInterval:60}") long sendMetricsInterval) {
        return UnleashConfig.builder()
                .appName(appName)
                .instanceId(getInstanceId())
                .unleashAPI(endpoint)
                .fetchTogglesInterval(fetchTogglesInterval)
                .sendMetricsInterval(sendMetricsInterval)
                .build();
    }

    @Bean
    public Unleash unleash(UnleashConfig unleashConfig){
        return new DefaultUnleash(unleashConfig, new DeviceClassStrategy());
    }


    @Bean
    public UnleashContextProvider unleashContextProvider(UserAgentAnalyzer userAgentAnalyzer){
        return new UnleashContextProvider(userAgentAnalyzer);
    }

    private String getInstanceId() {
        try {
            return InetAddress.getLocalHost().getHostName();
        } catch (UnknownHostException e) {
            return "Unknown";
        }
    }
}
