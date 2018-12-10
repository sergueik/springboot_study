package org.hanbo.boot.rest.controllers;

import java.util.ArrayList;
import java.util.List;

import org.hanbo.boot.rest.models.CarModel;
import org.hanbo.boot.rest.models.GenericResponse;
import org.hanbo.boot.rest.repository.CarRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestMethod;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
public class SampleController
{
   @Autowired
   private CarRepository carRepo;
   
   @RequestMapping(value="/public/addCar", method = RequestMethod.POST)
   public ResponseEntity<GenericResponse> addCar(
      @RequestBody
      CarModel carToAdd)
   {
      GenericResponse retMsg = new GenericResponse();
      if (carToAdd != null)
      {
         try
         {
            carRepo.addCar(carToAdd);
            
            retMsg.setSuccess(true);
            retMsg.setStatusMsg("Operation is successful.");
         }
         catch (Exception ex)
         {
            ex.printStackTrace();
            retMsg.setSuccess(false);
            retMsg.setStatusMsg("Exception occurred.");
         }
      }
      else
      {
         retMsg.setSuccess(false);
         retMsg.setStatusMsg("No valid car model object to be added");
      }
      
      ResponseEntity<GenericResponse> retVal;
      retVal = ResponseEntity.ok(retMsg);
      return retVal;
   }
   
   @RequestMapping(value="/public/getCars", method = RequestMethod.GET)
   public ResponseEntity<List<CarModel>> getCars(
      @RequestParam("make")
      String make,
      @RequestParam("startYear")
      int startYear,
      @RequestParam("endYear")
      int endYear)
   {
      List<CarModel> foundCars
         = carRepo.findCar(make, startYear, endYear);
      
      if (foundCars == null) {
         foundCars = new ArrayList<CarModel>();
      }
      
      ResponseEntity<List<CarModel>> retVal;
      retVal = ResponseEntity.ok(foundCars);
      return retVal;
   }
}
