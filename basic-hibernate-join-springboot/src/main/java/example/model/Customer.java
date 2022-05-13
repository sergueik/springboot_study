package example.model;

import java.util.List;

import javax.persistence.CascadeType;
import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.Id;
import javax.persistence.JoinColumn;
import javax.persistence.OneToMany;
import javax.persistence.OneToOne;
import javax.persistence.Table;

@Entity
@Table(name = "customer")
public class Customer {
	@Id
	@Column(name = "cid")
	private int customerId;
	@Column(name = "cname", length = 15)
	private String customerName;

	@OneToMany(targetEntity = Item.class, cascade = CascadeType.ALL)
	@JoinColumn(name = "cid", referencedColumnName = "cid")
	private List<Item> items;

	// NOTE: "OneToOne" will lead to invalid query:
	/*
	 Hibernate:
	  select
	      customer0_.cname as col_0_0_,
	      address2_.acity as col_1_0_,
	      items1_.iname as col_2_0_,
	      items1_.iprice as col_3_0_
	  from
	      customer customer0_
	  left outer join
	      item items1_
	          on customer0_.cid=items1_.cid
	  inner join
	      address address2_
	          on customer0_.cid=address2_.aid
	*/
	@OneToOne(targetEntity = Address.class, cascade = CascadeType.ALL)
	@JoinColumn(name = "cid", referencedColumnName = "cid")
	private Address address;

	public int getCustomerId() {
		return customerId;
	}

	public void setCustomerId(int value) {
		customerId = value;
	}

	public String getCustomerName() {
		return customerName;
	}

	public void setCustomerName(String value) {
		customerName = value;
	}

	public List<Item> getItems() {
		return items;
	}

	public void setItems(List<Item> value) {
		items = value;
	}

	public Address getAddress() {
		return address;
	}

	public void setAddress(Address value) {
		address = value;
	}

}
