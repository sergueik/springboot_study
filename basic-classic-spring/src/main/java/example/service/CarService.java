package example.service;

import java.util.List;

// origin: https://github.com/xvitcoder/spring-mvc-angularjs
public interface CarService {
	public List<String> getAllCars();

	public void addCar(String car);

	public void deleteCar(String car);

	public void deleteAll();
}
