package example;

/**
 * Common utilities class for CustomJob 
 * 
 * @author: Serguei Kouzmine (kouzmine_serguei@yahoo.com)
 */

public class Utils {

	private static Utils instance = new Utils();

	private boolean debug = false;
	private String info = null;

	public String getInfo() {
		return info;
	}

	public void setInfo(String value) {
		info = value;
	}

	public void setDebug(boolean value) {
		debug = value;
	}

	public boolean getDebug() {
		return debug;
	}

	private Utils() {
	}

	public static Utils getInstance() {
		return instance;
	}

}
