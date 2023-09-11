package example.component;

import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.annotation.Autowired;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import org.springframework.beans.factory.annotation.Value;
import org.springframework.boot.context.event.ApplicationReadyEvent;
import org.springframework.context.event.EventListener;
import org.springframework.stereotype.Component;

import example.repository.UserRepository;
import example.service.WriterService;

@Component
public class TaskComponent {

	@Autowired
	DataComponent data;

	@Autowired
	private UserRepository userRepository;


	@EventListener(ApplicationReadyEvent.class)
	public void ready() {

		new WriterService(userRepository, data, "Writer-1", 1).start();
	// 	new WriterService(data, "Writer-2", 2).start();
	}
}
