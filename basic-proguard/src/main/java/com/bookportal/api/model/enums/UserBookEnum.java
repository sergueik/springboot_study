package com.bookportal.api.model.enums;

public enum UserBookEnum {
    WILL_READ("0"), HAVE_READ("1");

    private final String value;

    UserBookEnum(String value) {
        this.value = value;
    }

    public String getValue() {
        return value;
    }

    public static boolean isExist(String val) {
        return val.equals(WILL_READ.value) || val.equals(HAVE_READ.value);
    }

    public static UserBookEnum findByValue(String value) {
        if (UserBookEnum.WILL_READ.getValue().equals(value)) {
            return UserBookEnum.WILL_READ;
        }
        return UserBookEnum.HAVE_READ;
    }
}
