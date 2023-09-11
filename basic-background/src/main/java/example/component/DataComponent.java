package example.component;

import java.util.Map;
/**
 * Copyright 2023 Serguei Kouzmine
 */
import java.util.concurrent.ConcurrentHashMap;
import example.domain.Gender;
import example.domain.User;

import org.springframework.stereotype.Component;

@SuppressWarnings("serial")
@Component
// NOTE: java has no pointers. What we reallly need is the reference to the
// whole rowset in memory
public class DataComponent extends ConcurrentHashMap<Long, Map<Long, User>> {

}
