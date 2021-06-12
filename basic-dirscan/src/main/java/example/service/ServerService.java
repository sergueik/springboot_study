package example.service;
/**
 * Copyright 2021 Serguei Kouzmine
 */

import java.util.ArrayList;
import java.util.List;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;

import example.component.ServerComponent;

@Service
public class ServerService {

	// @Autowired
	@Value("${example.ServerComponent.cofigFile:hosts.txt}")
	private String configFile;

	// @Autowired
	private ServerComponent serverComponent;

	public ServerService() {

	}

	public ServerService(String configFile) {
		this.configFile = configFile;
		this.serverComponent = new ServerComponent(configFile);
	}

	public List<String> getServers() {
		return serverComponent.getServers();
	}
}