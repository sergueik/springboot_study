package com.yxrkt.seltrace.junit5;

import com.yxrkt.seltrace.AgentApi;
import org.junit.jupiter.api.extension.BeforeEachCallback;
import org.junit.jupiter.api.extension.AfterEachCallback;
import org.junit.jupiter.api.extension.ExtensionContext;

public class TestIdExtension implements BeforeEachCallback, AfterEachCallback {
    @Override
    public void beforeEach(ExtensionContext ctx) {
        String id = ctx.getRequiredTestClass().getName() + "#" + ctx.getRequiredTestMethod().getName();
        AgentApi.setTestId(id);
    }
    @Override
    public void afterEach(ExtensionContext ctx) {
        AgentApi.clear();
    }
}
