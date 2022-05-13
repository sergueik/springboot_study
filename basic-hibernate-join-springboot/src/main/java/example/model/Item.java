package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "item")
public class Item {
	@Id
	@Column(name = "iid")
	private int itemId;
	@Column(name = "iname")
	private String itemName;
	@Column(name = "iprice")
	private int price;

	public int getItemId() {
		return itemId;
	}

	public void setItemId(int value) {
		itemId = value;
	}

	public String getItemName() {
		return itemName;
	}

	public void setItemName(String value) {
		itemName = value;
	}

	public int getPrice() {
		return price;
	}

	public void setPrice(int value) {
		price = value;
	}

}
