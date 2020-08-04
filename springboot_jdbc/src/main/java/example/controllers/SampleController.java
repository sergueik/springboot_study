package example.controllers;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import example.models.CarModel;
import example.models.GenericResponse;
import example.repository.CarRepository;

@RestController
public class SampleController {
	@Autowired
	private CarRepository carRepo;

	private CarModel carToAdd;

	// https://stackoverflow.com/questions/33796218/content-type-application-x-www-form-urlencodedcharset-utf-8-not-supported-for
	@RequestMapping(value = "/public/addCar", method = RequestMethod.POST, produces = {
			"application/json",
			"application/xml" }, consumes = { "application/x-www-form-urlencoded" })
	public ResponseEntity<GenericResponse> addCar(
			@RequestParam Map<String, String> body) {
		carToAdd = new CarModel();
		carToAdd.setModel(body.get("model"));
		carToAdd.setMaker(body.get("maker"));
		carToAdd.setYearOfManufacturing(
				Integer.parseInt(body.get("yearOfManufacturing")));
		GenericResponse retMsg = new GenericResponse();
		if (carToAdd != null) {
			try {
				carRepo.addCar(carToAdd);

				retMsg.setSuccess(true);
				retMsg.setStatusMsg("Operation is successful.");
			} catch (Exception ex) {
				ex.printStackTrace();
				retMsg.setSuccess(false);
				retMsg.setStatusMsg("Exception occurred.");
			}
		} else {
			retMsg.setSuccess(false);
			retMsg.setStatusMsg("No valid car model object to be added");
		}

		ResponseEntity<GenericResponse> retVal;
		retVal = ResponseEntity.ok(retMsg);
		return retVal;
	}

	@RequestMapping(value = "/public/getCars", method = RequestMethod.GET)
	public ResponseEntity<List<CarModel>> getCars(
			@RequestParam("make") String make,
			@RequestParam("startYear") int startYear,
			@RequestParam("endYear") int endYear) {
		List<CarModel> foundCars = carRepo.findCar(make, startYear, endYear);

		if (foundCars == null) {
			foundCars = new ArrayList<CarModel>();
		}

		ResponseEntity<List<CarModel>> retVal;
		retVal = ResponseEntity.ok(foundCars);
		return retVal;
	}
}

