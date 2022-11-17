package example.model.grafana;

// generated via https://json2csharp.com/code-converters/json-to-pojo

import java.util.ArrayList;
import java.util.List;
import example.model.grafana.AnnotationEntry;

import com.google.gson.annotations.SerializedName;
import example.model.grafana.AnnotationEntry
;//generated via https://json2csharp.com/code-converters/json-to-pojo

public class Annotations {
	@SerializedName("list")
	List<AnnotationEntry> list = new ArrayList<>();

	public List<AnnotationEntry> getList() {
		return list;
	}

	public void setList(List<AnnotationEntry> data) {
		list = data;
	}

}
/*
the default generated  class name List is to be avoided:

import java.util.ArrayList;
import java.util.List;

import com.google.gson.annotations.SerializedName;
// NOTE: The import example.model.grafana.List collides with another import statement
// import example.model.grafana.List;

public class Annotations {
	// when declaring a example.model.grafana.List,
	// use full package class name in the source
	// and comment the conflicting import
	@SerializedName("list")
	List<example.model.grafana.List> list = new ArrayList<>();

	public List<example.model.grafana.List> getList() {
		return list;
	}

	public void setList(List<example.model.grafana.List> data) {
		list = data;
	}

}


*/
