package example.model;

public class Dish {

	String name;

	public Dish(String value) {
		name = value;
	}

	public Dish() {
	}

	public String getName() {
		return name;
	}

	@Override
	public String toString() {
		return "Dish {" + "\"name\": " + "\"" + this.name + "\"" + '}';
	}

}