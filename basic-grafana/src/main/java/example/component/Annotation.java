package example.component;

public class Annotation {

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDatasource() {
		return datasource;
	}

	public void setDatasource(String datasource) {
		this.datasource = datasource;
	}

	public String getIconColor() {
		return iconColor;
	}

	public void setIconColor(String iconColor) {
		this.iconColor = iconColor;
	}

	public boolean getEnable() {
		return enable;
	}

	public void setEnable(boolean enable) {
		this.enable = enable;
	}

	public String getQuery() {
		return query;
	}

	public void setQuery(String query) {
		this.query = query;
	}

	private String name;
	private String datasource;
	private String iconColor;
	private boolean enable;
	private String query;
}
