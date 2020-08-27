package example.controller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.ResponseBody;

import example.service.CarService;

import java.util.List;

// origin: https://github.com/xvitcoder/spring-mvc-angularjs

@Controller
@RequestMapping("/cars")
public class CarController {

	@Autowired
	private CarService carService;

	@RequestMapping("/carlist.json")
	public @ResponseBody List<String> getCarList() {
		return carService.getAllCars();
	}

	@RequestMapping(value = "/addCar/{car}", method = RequestMethod.POST)
	public @ResponseBody void addCar(@PathVariable("car") String car) {
		carService.addCar(car);
	}

	@RequestMapping(value = "/removeCar/{car}", method = RequestMethod.DELETE)
	public @ResponseBody void removeCar(@PathVariable("car") String car) {
		carService.deleteCar(car);
	}

	@RequestMapping(value = "/removeAllCars", method = RequestMethod.DELETE)
	public @ResponseBody void removeAllCars() {
		carService.deleteAll();
	}

	@RequestMapping("/layout")
	public String getCarPartialPage() {
		return "cars/layout";
	}
}
