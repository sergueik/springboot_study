package com.example.dao;

import com.example.model.Rest;
import com.example.model.RestResult;

import java.util.List;

public interface RestDao {
	List<RestResult> getAll();

	RestResult getRestById(int id);

	void addRest(Rest rest);

	void updateRest(Rest rest, int id);

	void deleteRestById(int id);

	int latestInput();

}
