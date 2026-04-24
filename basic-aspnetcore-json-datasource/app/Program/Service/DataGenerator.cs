using System;

public static class DataGenerator {
	
	public static int counter = 0;
	
	public static double NextValue() {
	    counter++;
	    return Math.Sin(counter / 10.0) + 0.2 * Math.Sin(counter) + (counter / 100.0); 
	}
}

