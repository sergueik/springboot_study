package example.service;

import java.util.List;

import example.beans.Train;

//origin: https://github.com/xvitcoder/spring-mvc-angularjs

public interface TrainService {
	public List<Train> getAllTrains();

	public Train getTrainById(Long id);

	public void addTrain(Train train);

	public void deleteTrainById(Long id);

	public void deleteAll();

	public void updateTrain(Train train);
}
