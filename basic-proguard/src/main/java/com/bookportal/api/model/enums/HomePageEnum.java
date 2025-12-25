package com.bookportal.api.model.enums;

import com.bookportal.api.exception.CustomNotFoundException;

import java.util.Arrays;

public enum HomePageEnum {

    RECOMMENDED_BOOK("0"),
    RECOMMENDED_LIST("1"),
    EDITORS_CHOICE("2");

    private final String type;

    HomePageEnum(String type) {
        this.type = type;
    }

    public String getValue() {
        return type;
    }

    public static HomePageEnum findByType(String abbr) {
        return Arrays.stream(values())
                .filter(value -> value.type.equals(abbr))
                .findFirst()
                .orElseThrow(() -> new CustomNotFoundException(ExceptionItemsEnum.TYPE.getValue()));
    }

    public static HomePageEnum[] getEnums(){
        return HomePageEnum.class.getEnumConstants();
    }
}
