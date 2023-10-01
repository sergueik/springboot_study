package example.model;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

public class Order {

	private Date date = new Date();
	private String name;
	private List<Dish> dishes = new ArrayList<>();

	public List<Dish> getDishes() {
		return dishes;
	}

	public Date getDate() {
		return date;
	}

	public void SetDish(Dish value) {
		dishes.add(value);
	}

	public void SetDishes(List<Dish> value) {
		dishes = value;
	}

	public String getName() {
		return name;
	}

	public void setName(String value) {
		name = value;
	}

}
