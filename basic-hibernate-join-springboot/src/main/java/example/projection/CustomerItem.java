package example.projection;

public class CustomerItem {

	private String customerName;
	private String customerCity;
	// private String abbreviation;
	private String itemName;
	private int price;

	public CustomerItem() {

	}

	public CustomerItem(String customerName, String customerCity, String itemName,
			int price) {

		this.customerName = customerName;
		this.customerCity = customerCity;
		this.itemName = itemName;
		this.price = price;

	}

	public String getCustomerName() {
		return customerName;
	}

	public void setCustomerName(String value) {
		customerName = value;
	}

	public String getCustomerCity() {
		return customerCity;
	}

	public void setCustomerCity(String value) {
		customerCity = value;
	}

	// public String getAbbreviation() {
	// return abbreviation;
	// }

	// public void setAbbreviation(String value) {
	// abbreviation = value;
	// }

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
