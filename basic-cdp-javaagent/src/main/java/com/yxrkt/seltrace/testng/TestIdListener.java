package com.yxrkt.seltrace.testng;

import com.yxrkt.seltrace.AgentApi;
import org.testng.IInvokedMethod;
import org.testng.IInvokedMethodListener;
import org.testng.ITestResult;

public class TestIdListener implements IInvokedMethodListener {
    public void beforeInvocation(IInvokedMethod m, ITestResult tr) {
        AgentApi.setTestId(tr.getTestClass().getName() + "#" + tr.getMethod().getMethodName());
    }
    public void afterInvocation(IInvokedMethod m, ITestResult tr) {
        AgentApi.clear();
    }
}
