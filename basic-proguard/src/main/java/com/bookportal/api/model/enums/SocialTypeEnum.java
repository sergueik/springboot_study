package com.bookportal.api.model.enums;

public enum SocialTypeEnum {
    FACEBOOK("1"), GOOGLE("0");

    private final String value;

    SocialTypeEnum(String value) {
        this.value = value;
    }

    public static boolean isExist(String val) {
        return (val.equals(FACEBOOK.value)) || (val.equals(GOOGLE.value));
    }

    public String getValue() {
        return value;
    }

    public static SocialTypeEnum findByValue(String value) {
        if (value.equals(SocialTypeEnum.FACEBOOK.getValue())) {
            return SocialTypeEnum.FACEBOOK;
        }
        return SocialTypeEnum.GOOGLE;
    }
}
