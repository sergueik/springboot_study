package com.yxrkt.seltrace.junit4;

import com.yxrkt.seltrace.AgentApi;
import org.junit.rules.TestWatcher;
import org.junit.runner.Description;

public class TestIdRule extends TestWatcher {
    @Override
    protected void starting(Description description) {
        AgentApi.setTestId(description.getClassName() + "#" + description.getMethodName());
    }
    @Override
    protected void finished(Description description) {
        AgentApi.clear();
    }
}
