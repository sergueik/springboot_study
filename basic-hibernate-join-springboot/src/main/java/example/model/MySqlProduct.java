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

	// NOTE: need to have allproperties explicitly
	private String name;
	private int qty;
	private double price;

	public int getId() {
		return id;
	}

	public void setId(int id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public int getQty() {
		return qty;
	}

	public void setQty(int qty) {
		this.qty = qty;
	}

	public double getPrice() {
		return price;
	}

	public void setPrice(double price) {
		this.price = price;
	}

	public MySqlProduct(int id, String name, int qty, double price) {
		super(id, name, qty, price);
		this.id = id;
		this.name = name;
		this.qty = qty;
		this.price = price;
	}

	public MySqlProduct() {
		super();
	}
}

