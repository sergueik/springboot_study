package example.component;

/**
 * Copyright 2023 Serguei Kouzmine
 */
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.stereotype.Component;

@SuppressWarnings("serial")
@Component
public class DataComponent extends ConcurrentHashMap<Integer, String> {

}
