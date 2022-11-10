package example.model.grafana;

import java.util.ArrayList;
import java.util.List;

// generated via https://json2csharp.com/code-converters/json-to-pojo
// modified to enclose List<AnnotationEntry> instead of List<List> 
public class Annotations {
	List<AnnotationEntry> annotationEntries = new ArrayList<>();

	public List<AnnotationEntry> getAnnotationEntries() {
		return annotationEntries;
	}

	public void setAnnotationEntries(List<AnnotationEntry> data) {
		annotationEntries = data;
	}

}
