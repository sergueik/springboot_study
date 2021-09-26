package com.pluralsight.websockets.message;

public class MessageType {
    public static final int JOIN = 1;
    public static final int MESSAGE = 2;
    public static final int GETUSERS = 3;
    public static final int USERLIST = 4;
    public static final int GETMESSAGES = 5; //not used as GetUsers returns the messages too
    public static final int MESSAGELIST = 6;
}
