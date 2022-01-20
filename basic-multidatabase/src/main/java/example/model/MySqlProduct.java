package example.model;

/**
 * Copyright 2022 Serguei Kouzmine
 */
import javax.persistence.Entity;
import javax.persistence.Id;

@Entity
public class MySqlProduct extends Product {
	@Id
	private int id;

	public MySqlProduct(int id, String name, int qty, double price) {
		super(id, name, qty, price);
	}
	public MySqlProduct(){
		super();
	}
}
