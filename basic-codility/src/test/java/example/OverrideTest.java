package example;
/**
 * Copyright 2024 Serguei Kouzmine
 */

import static org.hamcrest.CoreMatchers.is;
import static org.hamcrest.CoreMatchers.not;
import static org.hamcrest.MatcherAssert.assertThat;

import org.junit.Test;

// https://www.baeldung.com/java-method-overload-override

public class OverrideTest {

	private boolean debug = true;
	private Vehicle car1 = new Car();
	// car1.park() is not available
	private Car car2 = new Car();
	private Vehicle vehicle = new Vehicle();

	// https://stackoverflow.com/questions/5811515/using-not-operation-in-hamcrest
	@Test
	public void test1() {
		assertThat(car1.stop(), is(vehicle.stop()));
		assertThat(car1.accelerate(100), not(vehicle.accelerate(100)));
	}

	@Test
	public void test2() {
		assertThat(car2.stop(), is(vehicle.stop()));
		assertThat(car2.accelerate(100), is(not(vehicle.accelerate(100))));
	}
}
