package com.yxrkt.agent;

import java.util.HashMap;
import java.util.Map;

public class Config {
    private static Map<String,String> cfg = new HashMap<>();
    private static String out = "/tmp/seltrace.ndjson";
    private static boolean cdp = true;
    private static boolean stack = false;

    public static void init(String args) {
        if (args == null || args.trim().isEmpty()) return;
        String[] parts = args.split(",");
        for (String p: parts) {
            String[] kv = p.split("=",2);
            if (kv.length==2) cfg.put(kv[0].trim(), kv[1].trim());
        }
        out = cfg.getOrDefault("out", out);
        cdp = Boolean.parseBoolean(cfg.getOrDefault("cdp", String.valueOf(cdp)));
        stack = Boolean.parseBoolean(cfg.getOrDefault("stack", String.valueOf(stack)));
    }

    public static String outPath() { return out; }
    public static boolean isCdpEnabled() { return cdp; }
    public static boolean stackEnabled() { return stack; }
}
