package example.model;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "products")
public class MongoProduct extends Product {
	public MongoProduct(int id, String name, int qty, double price) {
		super(id, name, qty, price);
	}

	@Id
	private int id;
	public MongoProduct(){
		super();
	}
}
