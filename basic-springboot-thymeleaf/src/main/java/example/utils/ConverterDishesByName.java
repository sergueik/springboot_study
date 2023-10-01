package example.utils;

import java.util.HashMap;
import java.util.Map;

import org.springframework.core.convert.converter.Converter;
import org.springframework.stereotype.Component;

import example.model.Dish;

@Component
public class ConverterDishesByName implements Converter<String, Dish> {
	private Map<String, Dish> dishesMap = new HashMap<>();

	public ConverterDishesByName() {
		dishesMap.put("burger", new Dish("Burger"));
		dishesMap.put("cola", new Dish("Cola"));
		dishesMap.put("chicken", new Dish("Chicken"));
		dishesMap.put("tea", new Dish("Tea"));
	}

	@Override
	public Dish convert(String name) {
		return dishesMap.get(name);
	}

}
