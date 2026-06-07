package com.yxrkt.agent.bus;

import com.eclipsesource.json.JsonObject;
import com.eclipsesource.json.JsonValue;

public class Json {
    private final JsonObject jo;
    public Json(JsonObject jo) { this.jo = jo; }
    public static Json obj() { return new Json(new JsonObject()); }
    public Json put(String k, Object v) {
        if (v==null) { jo.add(k, JsonValue.NULL); return this; }
        if (v instanceof Number) { jo.add(k, ((Number)v).toString()); return this; }
        if (v instanceof Boolean) { jo.add(k, (Boolean)v); return this; }
        jo.add(k, String.valueOf(v));
        return this;
    }
    public String toString() { return jo.toString(); }
}
