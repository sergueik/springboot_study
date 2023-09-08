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

import example.service.ReaderService;
import example.service.WriterService;

@Component
public class TaskComponent {

	@Autowired
	DataComponent data;

	@EventListener(ApplicationReadyEvent.class)
	public void ready() {

		new WriterService(data, "Writer-1", 1).start();
		new WriterService(data, "Writer-2", 2).start();
		/*
				for (int i = 1; i <= 5; i++) {
					new ReaderService(data, "Reader-" + i).start();
				}
		*/
	}
}
