package com.example.dao;

import com.example.model.BackendData;

import java.util.List;

public interface BackendDataDao {
	List<BackendData> getAll();
	BackendData getBackendDataById(int id);
	void addBackendData(BackendData rest);
	void updateBackendData(BackendData rest, int id);
	void deleteBackendDataById(int id);
	int latestInput();
}
