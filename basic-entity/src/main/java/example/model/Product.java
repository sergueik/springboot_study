package example.model;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;

// @Entity(name = "#{com.example.crawler.repository.ProductRepositoryCustom.getEntityName()}")
// SQL error or missing database (unrecognized token: "#")
// @Entity(name = "product")
@Entity
public class Product {
	@Id
	// commenting leads to error:
	// ids for this class must be manually assigned before calling save()
	// @GeneratedValue
	// @GeneratedValue(strategy = GenerationType.AUTO)
	// use strategy="AUTO", Hibernate will generate a table called
	// hibernate_sequence
	// and fail in runtime if the table not created
	@GeneratedValue(strategy = GenerationType.TABLE)
	// create table hibernate_sequences (sequence_name varchar(255) not null,
	// next_val bigint, primary key (sequence_name))
	// org.hibernate.dialect.identity.IdentityColumnSupportImpl does not support
	// identity key generation
	@Column(name = "ID")
	private Long id;
	@Column(name = "NAME")
	private String name;
	@Column(name = "PRICE")
	private String price;
	@Column(name = "DESCRIPTION")
	private String description;
	@Column(name = "EXTRA_INFORMATION")
	private String extraInformation;

	public Product() {
	}

	public Product(String name, String price, String description, String extraInformation) {
		this.name = name;
		this.price = price;
		this.description = description;
		this.extraInformation = extraInformation;
	}

	public Long getId() {
		return id;
	}

	public void setId(Long id) {
		this.id = id;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getPrice() {
		return price;
	}

	public void setPrice(String price) {
		this.price = price;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getExtraInformation() {
		return extraInformation;
	}

	public void setExtraInformation(String extraInformation) {
		this.extraInformation = extraInformation;
	}
}
