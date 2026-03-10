package example;

import java.util.List;

public record Conference(String name, int year, String[] dates, String location, List<Session> sessions) {
}
