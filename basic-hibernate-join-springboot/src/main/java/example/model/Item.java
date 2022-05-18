package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "item")
public class Item {

	private int itemId;	
	private String itemName;
	private int price;

	@Id
	@Column(name = "iid")
	public int getItemId() {
		return itemId;
	}

	public void setItemId(int value) {
		itemId = value;
	}

	@Column(name = "iname", nullable = false, length = 10)
	public String getItemName() {
		return itemName;
	}

	public void setItemName(String value) {
		itemName = value;
	}

	@Column(name = "iprice")
	public int getPrice() {
		return price;
	}

	public void setPrice(int value) {
		price = value;
	}

}
