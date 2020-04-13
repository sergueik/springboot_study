package example.service;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import example.dao.BackendDataDao;
import example.model.BackendData;

import java.util.List;

@Service
public class RestServiceImp implements RestService {

	@Autowired
	private BackendDataDao backendDataDao;

	@Override
	public List<BackendData> getAll() {
		return backendDataDao.getAll();
	}

	@Override
	public BackendData getBackendDataById(int id) {
		return backendDataDao.getBackendDataById(id);
	}

	@Override
	public void addBackendData(BackendData rest) {
		backendDataDao.addBackendData(rest);
	}

	@Override
	public void updateBackendData(BackendData rest, int id) {
		backendDataDao.updateBackendData(rest, id);
	}

	@Override
	public void deleteBackendDataById(int id) {
		backendDataDao.deleteBackendDataById(id);
	}

	@Override
	public int latestInput() {
		return backendDataDao.latestInput();
	}

}