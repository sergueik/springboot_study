package example.service;

import java.util.List;

import example.beans.RailwayStation;

//origin: https://github.com/xvitcoder/spring-mvc-angularjs

public interface RailwayStationService {

	public List<RailwayStation> getAllRailwayStations();

	public RailwayStation getRailwayStationById(Long id);

	public void addRailwayStation(RailwayStation RailwayStation);

	public void deleteRailwayStationById(Long id);

	public void deleteAll();

	public void updateRailwayStation(RailwayStation RailwayStation);
}
