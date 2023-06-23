package example;

/**
 * Copyright 2023 Serguei Kouzmine
 */

import java.util.Map;

public interface PropertyUpdater {

	public String getConfiguration();
	public void setConfiguration(String value);
	public void setProperties(Map<String, Object> value);
	public void setTrim(boolean value);
	public void updateConfiguration();

}
