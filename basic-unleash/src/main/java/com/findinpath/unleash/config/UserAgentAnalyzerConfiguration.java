package com.findinpath.unleash.config;

import nl.basjes.parse.useragent.UserAgentAnalyzer;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

/**
 * The yauaa user agent analyzer is used for extracting the
 * device class out of the `User-Agent` request header.
 */
@Configuration
public class UserAgentAnalyzerConfiguration {

    @Bean
    public UserAgentAnalyzer userAgentAnalyzer(){
        return UserAgentAnalyzer
                .newBuilder()
                .withField("DeviceClass")
                .build();
    }
}
