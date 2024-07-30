package example;

public class Car extends Vehicle {

	@Override
	public String accelerate(long mph) {
		return "The car accelerates at : " + mph + " MPH.";
	}

	public String park() {
		return "The car parks.";
	}

}