package com.yxrkt.agent.util;

public class StackUtils {
    public static String compact() {
        StackTraceElement[] st = Thread.currentThread().getStackTrace();
        StringBuilder sb = new StringBuilder();
        for (int i=3;i<Math.min(st.length, 10);i++) {
            sb.append(st[i].getClassName()).append(".").append(st[i].getMethodName()).append("():").append(st[i].getLineNumber());
            if (i<Math.min(st.length,10)-1) sb.append(" > ");
        }
        return sb.toString();
    }
}
