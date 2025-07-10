package example.controller;
import org.mozilla.javascript.Context;

public enum RhinoVersion {
    VERSION_1_7(Context.VERSION_1_7),
    VERSION_ES6(Context.VERSION_ES6),
    VERSION_1_8(Context.VERSION_1_8);

    private final int value;

    RhinoVersion(int value) {
        this.value = value;
    }

    public int getValue() {
        return value;
    }
}