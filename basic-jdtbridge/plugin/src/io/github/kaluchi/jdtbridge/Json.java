package io.github.kaluchi.jdtbridge;

/**
 * Lightweight JSON builder for constructing JSON objects and arrays.
 * All string values are automatically escaped per RFC 8259.
 */
class Json {

    private final StringBuilder sb;
    private boolean first = true;
    private final char close;

    private Json(char open, char close) {
        this.sb = new StringBuilder();
        this.sb.append(open);
        this.close = close;
    }

    static Json object() {
        return new Json('{', '}');
    }

    static Json array() {
        return new Json('[', ']');
    }

    /** Convenience: creates {"error":"message"} */
    static String error(String message) {
        return object().put("error", message).toString();
    }

    // ---- Object methods ----

    Json put(String key, String value) {
        comma();
        appendKey(key);
        if (value == null) {
            sb.append("null");
        } else {
            sb.append('"').append(escape(value)).append('"');
        }
        return this;
    }

    Json put(String key, int value) {
        comma();
        appendKey(key);
        sb.append(value);
        return this;
    }

    Json put(String key, long value) {
        comma();
        appendKey(key);
        sb.append(value);
        return this;
    }

    Json put(String key, double value) {
        comma();
        appendKey(key);
        sb.append(value);
        return this;
    }

    Json put(String key, boolean value) {
        comma();
        appendKey(key);
        sb.append(value);
        return this;
    }

    Json put(String key, Json nested) {
        comma();
        appendKey(key);
        sb.append(nested != null ? nested.toString() : "null");
        return this;
    }

    /** Add key:value only if condition is true. */
    Json putIf(boolean condition, String key, String value) {
        if (condition) put(key, value);
        return this;
    }

    Json putIf(boolean condition, String key, boolean value) {
        if (condition) put(key, value);
        return this;
    }

    // ---- Array methods ----

    Json add(String value) {
        comma();
        if (value == null) {
            sb.append("null");
        } else {
            sb.append('"').append(escape(value)).append('"');
        }
        return this;
    }

    Json add(Json nested) {
        comma();
        sb.append(nested != null ? nested.toString() : "null");
        return this;
    }

    Json add(int value) {
        comma();
        sb.append(value);
        return this;
    }

    // ---- Output ----

    @Override
    public String toString() {
        return sb.toString() + close;
    }

    // ---- Internals ----

    private void comma() {
        if (!first) sb.append(',');
        first = false;
    }

    private void appendKey(String key) {
        sb.append('"').append(escape(key)).append("\":");
    }

    /** JSON string escaping — handles all control characters per RFC 8259. */
    static String escape(String s) {
        if (s == null) return "null";
        StringBuilder out = new StringBuilder(s.length());
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            switch (c) {
                case '"' -> out.append("\\\"");
                case '\\' -> out.append("\\\\");
                case '\n' -> out.append("\\n");
                case '\r' -> out.append("\\r");
                case '\t' -> out.append("\\t");
                case '\b' -> out.append("\\b");
                case '\f' -> out.append("\\f");
                default -> {
                    if (c < 0x20) {
                        out.append(String.format("\\u%04x", (int) c));
                    } else {
                        out.append(c);
                    }
                }
            }
        }
        return out.toString();
    }
}
