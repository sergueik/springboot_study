package com.example.dao;

import com.example.model.Rest;

import java.util.List;

public interface RestDao {
	List<Rest> getAll();
	Rest getRestById(int id);
	void addRest(Rest rest);
	void updateRest(Rest rest, int id);
	void deleteRestById(int id);
	int latestInput();
}
