package com.findinpath.unleash.context;

import com.findinpath.unleash.strategy.DeviceClassStrategy;
import nl.basjes.parse.useragent.UserAgent;
import nl.basjes.parse.useragent.UserAgentAnalyzer;
import nl.basjes.parse.useragent.classify.DeviceClass;
import no.finn.unleash.UnleashContext;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.context.request.RequestAttributes;
import org.springframework.web.context.request.RequestContextHolder;
import org.springframework.web.context.request.ServletRequestAttributes;

import javax.servlet.http.HttpServletRequest;
import java.util.Arrays;
import java.util.Optional;

/**
 * This class has the purpose of hiding the complexities of building
 * the <code>UnleashContext</code> needed in checking whether a
 * feature toggle is enabled or not.
 */
public class UnleashContextProvider {
    private static final String CONTEXT_KEY = "demo.unleashContext";

    private static final String USER_AGENT_HEADER_NAME = "User-Agent";

    private final UserAgentAnalyzer userAgentAnalyzer;

    private final UnleashContext appUnleashContext;

    public UnleashContextProvider(UserAgentAnalyzer userAgentAnalyzer){
        this.userAgentAnalyzer = userAgentAnalyzer;
        this.appUnleashContext = UnleashContext.builder().build();
    }

    public UnleashContext getUnleashContext(){
        var attrs = RequestContextHolder.getRequestAttributes();
        if (attrs == null){
            return appUnleashContext;
        }
        UnleashContext ctx = (UnleashContext) attrs.getAttribute(CONTEXT_KEY, RequestAttributes.SCOPE_REQUEST);
        if (ctx == null) {
            UnleashContext.Builder builder = UnleashContext.builder();
            Optional.ofNullable(getAuthentication())
                    .ifPresent(authentication -> builder.userId(authentication.getName()));
            builder.addProperty(DeviceClassStrategy.PARAM, getCurrentDeviceClass().getValue());

            ctx = builder.build();

            attrs.setAttribute(CONTEXT_KEY,ctx, RequestAttributes.SCOPE_REQUEST);
        }
        return ctx;
    }


    public void clearUnleashContext() {
        RequestAttributes attrs = RequestContextHolder.getRequestAttributes();
        if (attrs != null) {
            attrs.removeAttribute(CONTEXT_KEY, RequestAttributes.SCOPE_REQUEST);
        }
    }

    private Authentication getAuthentication(){
        return SecurityContextHolder.getContext().getAuthentication();
    }

    private DeviceClass getCurrentDeviceClass() {
        var request = getRequest();
        return request
                .flatMap(this::getUserAgentHeader)
                .map(this::extractDeviceClass)
                .orElse(DeviceClass.UNKNOWN);
    }

    private Optional<HttpServletRequest> getRequest() {
        return Optional.ofNullable(RequestContextHolder.getRequestAttributes())
                .map(requestAttributes -> ((ServletRequestAttributes) requestAttributes))
                .map(ServletRequestAttributes::getRequest);
    }

    private Optional<String> getUserAgentHeader(HttpServletRequest request) {
        return Optional.ofNullable(request.getHeader(USER_AGENT_HEADER_NAME));
    }

    private DeviceClass extractDeviceClass(String userAgentString) {
        if (userAgentString == null) {
            return null;
        }

        var userAgent = userAgentAnalyzer.parse(userAgentString);

        var deviceClassValue = userAgent.getValue(UserAgent.DEVICE_CLASS);
        return  Arrays.stream(DeviceClass.values())
                .filter(deviceClass -> deviceClass.getValue().equals(deviceClassValue))
                .findAny()
                .orElse(DeviceClass.UNKNOWN);
    }
}
