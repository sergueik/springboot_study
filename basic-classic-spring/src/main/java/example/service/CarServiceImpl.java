package example.service;

import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;

// origin: https://github.com/xvitcoder/spring-mvc-angularjs
@Service("carService")
public class CarServiceImpl implements CarService {
	private static List<String> carList = new ArrayList<>();

	@Override
	public List<String> getAllCars() {
		return carList;
	}

	@Override
	public void addCar(String car) {
		carList.add(car);
	}

	@Override
	public void deleteCar(String car) {
		if (carList.contains(car)) {
			carList.remove(car);
		}
	}

	@Override
	public void deleteAll() {
		carList.clear();
	}
}
