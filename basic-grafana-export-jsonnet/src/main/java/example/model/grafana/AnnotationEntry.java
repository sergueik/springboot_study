package example.model.grafana;

// generated via https://json2csharp.com/code-converters/json-to-pojo
// renamed from the "public class List" 
// which is probably not safe to define in pojo type 

public class AnnotationEntry {

	private float builtIn;
	private Datasource datasource;
	private boolean enable;
	private boolean hide;
	private String iconColor;
	private String name;
	private String type;
	private Target target;


	public float getBuiltIn() {
		return builtIn;
	}

	public Datasource getDatasource() {
		return datasource;
	}

	public boolean getEnable() {
		return enable;
	}

	public boolean getHide() {
		return hide;
	}

	public String getIconColor() {
		return iconColor;
	}

	public String getName() {
		return name;
	}

	public String getType() {
		return type;
	}

	public void setBuiltIn(float data) {
		builtIn = data;
	}

	public void setDatasource(Datasource data) {
		datasource = data;
	}

	public void setEnable(boolean data) {
		enable = data;
	}

	public void setHide(boolean hide) {
		this.hide = hide;
	}

	public void setIconColor(String data) {
		iconColor = data;
	}

	public void setName(String data) {
		name = data;
	}

	public void setType(String data) {
		type = data;
	}

	public Target getTarget() {
		return target;
	}

	public void setTarget(Target data) {
		target = data;
	}
}
