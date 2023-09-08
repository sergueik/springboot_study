package example.component;

/**
 * Copyright 2023 Serguei Kouzmine
 */
import java.util.concurrent.ConcurrentHashMap;
import example.domain.Gender;
import example.domain.User;

import org.springframework.stereotype.Component;

@SuppressWarnings("serial")
@Component
public class DataComponent extends ConcurrentHashMap<Long, User> {

}
