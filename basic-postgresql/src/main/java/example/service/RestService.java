package example.service;

import java.util.List;

import example.model.BackendData;

public interface RestService {
	List<BackendData> getAll();
	BackendData getBackendDataById(int id);
	void addBackendData(BackendData rest);
	void updateBackendData(BackendData rest, int id);
	void deleteBackendDataById(int id);
	int latestInput();
	List<BackendData> queryByIds(List<Integer> ids);
	List<BackendData> queryByIdsAndKeys(List<Integer> ids, List<String> keys);
	// PostgreSQL only
	List<BackendData> querySimilarToSetIds(List<Integer> ids);
	List<BackendData> querySimilarToSetKeys(List<String> keys);
	// MySQL only
	List<BackendData> queryRegexpOfSetIds(List<Integer> ids);
	List<BackendData> queryRegexpOfSetKeys(List<String> keys);
}
