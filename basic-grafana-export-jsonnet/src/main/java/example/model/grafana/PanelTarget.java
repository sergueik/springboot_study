package example.model.grafana;

import java.util.ArrayList;
import java.util.List;

// generated via https://json2csharp.com/code-converters/json-to-pojo
public class PanelTarget {

	public String refId;
	public String editorMode;
	public String expr;
	public String legendFormat;
	private boolean range;
	private Datasource datasource;

	public Datasource getDatasource() {
		return datasource;
	}

	public void setDatasource(Datasource data) {
		datasource = data;
	}

	public boolean isRange() {
		return range;
	}

	public void setRange(boolean data) {
		range = data;
	}

	public String getRefId() {
		return refId;
	}

	public void setRefId(String data) {
		refId = data;
	}

	public String getEditorMode() {
		return editorMode;
	}

	public void setEditorMode(String editorMode) {
		this.editorMode = editorMode;
	}

	public String getExpr() {
		return expr;
	}

	public void setExpr(String expr) {
		this.expr = expr;
	}

	public String getLegendFormat() {
		return legendFormat;
	}

	public void setLegendFormat(String legendFormat) {
		this.legendFormat = legendFormat;
	}

}
