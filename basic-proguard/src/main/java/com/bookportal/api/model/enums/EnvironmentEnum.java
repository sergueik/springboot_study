package com.bookportal.api.model.enums;

public enum EnvironmentEnum {
    ANDROID("0"), IOS("1");

    private final String value;

    EnvironmentEnum(String value) {
        this.value = value;
    }

    public static boolean isExist(String val) {
        return (val.equals(ANDROID.value)) || (val.equals(IOS.value));
    }

    public String getValue() {
        return value;
    }

    public static EnvironmentEnum findByValue(String value) {
        if (value.equals(EnvironmentEnum.ANDROID.getValue())) {
            return EnvironmentEnum.ANDROID;
        }
        return EnvironmentEnum.IOS;
    }
}
