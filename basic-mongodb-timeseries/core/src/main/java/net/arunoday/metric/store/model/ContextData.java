package net.arunoday.metric.store.model;

import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * Context information for events.
 * 
 * @author Aparna Chaudhary
 */
public class ContextData extends LinkedHashMap<String, Object> {

	private static final long serialVersionUID = 2338246646817230864L;

	/**
	 * Creates an empty object.
	 */
	public ContextData() {
	}

	public ContextData(int size) {
		super(size);
	}

	/**
	 * Public Constructor
	 * 
	 * @param key key under which to store
	 * @param value value to store
	 */
	public ContextData(String key, Object value) {
		put(key, value);
	}

	/**
	 * Creates a ContextData from a map.
	 * 
	 * @param m map to convert
	 */
	public ContextData(Map<String, Object> m) {
		super(m);
	}

	/**
	 * Converts a ContextData to a map.
	 * 
	 * @return the ContextData
	 */
	public Map<String, Object> toMap() {
		return new LinkedHashMap<String, Object>(this);
	}

	/**
	 * Deletes a field from this object.
	 * 
	 * @param key the field name to remove
	 * @return the object removed
	 */
	public Object removeField(String key) {
		return remove(key);
	}

	/**
	 * Checks if this object contains a given field
	 * 
	 * @param field field name
	 * @return if the field exists
	 */
	public boolean containsField(String field) {
		return super.containsKey(field);
	}

	/**
	 * Gets a value from this object
	 * 
	 * @param key field name
	 * @return the value
	 */
	public Object get(String key) {
		return super.get(key);
	}

	@Override
	@SuppressWarnings({ "rawtypes", "unchecked" })
	public void putAll(Map m) {
		for (Map.Entry entry : (Set<Map.Entry>) m.entrySet()) {
			put(entry.getKey().toString(), entry.getValue());
		}
	}

	public void putAll(ContextData o) {
		for (String k : o.keySet()) {
			put(k, o.get(k));
		}
	}

	/**
	 * Add a key/value pair to this object
	 * 
	 * @param key the field name
	 * @param val the field value
	 * @return <code>this</code>
	 */
	public ContextData append(String key, Object val) {
		put(key, val);
		return this;
	}

}
