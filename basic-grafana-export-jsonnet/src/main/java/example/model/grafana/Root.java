package example.model.grafana;

import java.util.ArrayList;

//generated via https://json2csharp.com/code-converters/json-to-pojo

public class Root {
	public ArrayList<Input> __inputs;
	public ArrayList<Require> __requires;
	private Annotations annotations;
	public boolean editable;
	public Object gnetId;
	public int graphTooltip;
	public Object id;
	public ArrayList<Object> links;
	public ArrayList<Panel> panels;
	public int schemaVersion;
	public String style;
	public ArrayList<Object> tags;
	public Templating templating;
	public Time time;
	public Timepicker timepicker;
	public String timezone;
	public String title;
	public String uid;
	public int version;

	public Annotations getAnnotations() {
		return annotations;
	}

	public void setAnnotations(Annotations data) {
		annotations = data;
	}

	public ArrayList<Panel> getPanels() {
		return panels;
	}

	public void setPanels(ArrayList<Panel> data) {
		panels = data;
	}

}
