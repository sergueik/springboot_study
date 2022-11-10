package example.model.grafana;

// TODO:
// com.fasterxml.jackson.databind.exc.InvalidDefinitionException: 
// No serializer found for class example.model.grafana.Heatmap 
// and no properties discovered to create BeanSerializer
// to avoid exception, disable SerializationFeature.FAIL_ON_EMPTY_BEANS
// through reference chain: 
// example.model.grafana.Root["panels"]->java.util.ArrayList[0]->example.model.grafana.Panel["heatmap"]
public class Heatmap {
	public Heatmap() {
	}
}
