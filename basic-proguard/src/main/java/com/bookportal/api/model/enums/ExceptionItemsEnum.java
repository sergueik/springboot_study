package com.bookportal.api.model.enums;

public enum ExceptionItemsEnum {
    BOOK("Book"),
    CATEGORY("Category"),
    COMMENT("Comment"),
    USER("User"),
    EDITOR("Editor"),
    QUOTE("Quote"),
    PUBLISHER("Publisher"),
    AUTHOR("Author"),
    FAVOURITE("Favourite"),
    TYPE("Type"),
    SOCIAL_TYPE("SocialType"),
    KEY("Key");

    private final String type;

    ExceptionItemsEnum(String type) {
        this.type = type;
    }

    public String getValue() {
        return type;
    }
}
