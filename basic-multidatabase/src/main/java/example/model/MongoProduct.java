package example.model;

/**
 * Copyright 2022 Serguei Kouzmine
 */

import org.springframework.data.annotation.Id;
import org.springframework.data.mongodb.core.mapping.Document;

@Document(collection = "products")
public class MongoProduct extends Product {
	@Id
	private int id;

	// NOTE: Both private java.lang.String example.model.Product.name and private
	// java.lang.String example.model.MongoProduct.name map to the same field name
	// name! Disambiguate using @Field annotation!
	// private String name;

	public int getId() {
		return super.getId();
	}

	public void setId(int id) {
		super.setId(id);
	}

	public String getName() {
		return super.getName();
	}

	public void setName(String name) {
		super.setName(name);
	}

	public int getQty() {
		return super.getQty();
	}

	public void setQty(int qty) {
		super.setQty(qty);
	}

	public double getPrice() {
		return super.getPrice();
	}

	public void setPrice(double price) {
		super.setPrice(price);
	}

	public MongoProduct() {
		super();
	}

	public MongoProduct(int id, String name, int qty, double price) {
		super(id, name, qty, price);
		this.id = id;
	}
}

