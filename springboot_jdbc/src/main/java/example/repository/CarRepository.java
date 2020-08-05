package example.repository;

import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.jdbc.core.namedparam.MapSqlParameterSource;
import org.springframework.jdbc.core.namedparam.NamedParameterJdbcTemplate;
import org.springframework.stereotype.Repository;
import org.springframework.transaction.annotation.Transactional;

import example.models.CarModel;

@Repository
public class CarRepository {
	@Autowired
	private NamedParameterJdbcTemplate sqlDao;
	private static Logger logger = LoggerFactory.getLogger(CarRepository.class);

	private final String addCar_sql = "INSERT INTO carinfo (yearofmanufacture, model, make, suggestedretailprice, fullprice, rebateamount, createdate, updatedate)"
			+ " VALUES (:yearOfManufacture, :model, :make, :suggestedRetailPrice, :fullPrice, :rebateAmount, :createdDate, :updatedDate)";
	/*
		private final String getCars_sql = "SELECT id," + " yearofmanufacture,"
				+ " model," + " make," + " suggestedretailprice," + " fullprice,"
				+ " rebateamount," + " createdate,"
				+ " updatedate FROM carinfo WHERE make = :make AND yearofmanufacture >= :startYear AND yearofmanufacture <= :endYear";
	*/
	private final String getCars_sql = "SELECT id," + " yearofmanufacture,"
			+ " model," + " make," + " suggestedretailprice," + " fullprice,"
			+ " rebateamount," + " createdate,"
			+ " updatedate FROM carinfo WHERE make = :make";
	private final String getAllCars_sql = "SELECT id," + " yearofmanufacture,"
			+ " model," + " make," + " suggestedretailprice," + " fullprice,"
			+ " rebateamount," + " createdate," + " updatedate FROM carinfo";

	@Transactional
	public void addCar(CarModel carToAdd) {
		if (carToAdd != null) {
			Map<String, Object> parameters = new HashMap<>();

			Date dateNow = new Date();

			parameters.put("yearOfManufacture", carToAdd.getYearOfManufacturing());
			parameters.put("model", carToAdd.getModel());
			parameters.put("make", carToAdd.getMaker());
			parameters.put("suggestedRetailPrice",
					carToAdd.getSuggestedRetailPrice());
			parameters.put("fullPrice", carToAdd.getFullPrice());
			parameters.put("rebateAmount", carToAdd.getRebateAmount());
			parameters.put("createdDate", dateNow);
			parameters.put("updatedDate", dateNow);

			logger.info("Update SQL: {}, parameters: {}", addCar_sql, parameters);

			int retVal = sqlDao.update(addCar_sql, parameters);
			logger.info("Rows updated: {}", retVal);
		} else {
			logger.info("Car to add is invalid. Null Object.");
		}
	}

	@Transactional
	public List<CarModel> getAllCars() {

		List<CarModel> foundObjs = sqlDao.query(getAllCars_sql, (rs) -> {
			List<CarModel> retVal = new ArrayList<>();
			if (rs != null) {
				while (rs.next()) {
					CarModel cm = new CarModel();
					cm.setYearOfManufacturing(rs.getInt("yearOfManufacture"));
					cm.setMaker(rs.getString("make"));
					cm.setModel(rs.getString("model"));
					cm.setSuggestedRetailPrice(rs.getFloat("suggestedretailprice"));
					cm.setFullPrice(rs.getFloat("fullprice"));
					cm.setRebateAmount(rs.getFloat("rebateamount"));
					retVal.add(cm);
				}
			}

			return retVal;
		});

		return foundObjs;
	}

	@Transactional
	public List<CarModel> findCar(String make, int startYear, int endYear) {
		List<CarModel> foundObjs = sqlDao.query(getCars_sql,
				/*
				(new MapSqlParameterSource("make", make))
						.addValue("startYear", startYear).addValue("endYear", endYear),
						
						*/
				new MapSqlParameterSource("make", make), (rs) -> {
					List<CarModel> retVal = new ArrayList<>();
					if (rs != null) {
						while (rs.next()) {
							CarModel cm = new CarModel();
							cm.setYearOfManufacturing(rs.getInt("yearOfManufacture"));
							cm.setMaker(rs.getString("make"));
							cm.setModel(rs.getString("model"));
							cm.setSuggestedRetailPrice(rs.getFloat("suggestedretailprice"));
							cm.setFullPrice(rs.getFloat("fullprice"));
							cm.setRebateAmount(rs.getFloat("rebateamount"));
							retVal.add(cm);
						}
					}

					return retVal;
				});

		return foundObjs;
	}
}
