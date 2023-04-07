package example.model;

import java.io.Serializable;

import javax.persistence.Column;
import javax.persistence.Entity;
import javax.persistence.GeneratedValue;
import javax.persistence.GenerationType;
import javax.persistence.Id;
import javax.persistence.Table;

@Entity
@Table(name = "customers")
public class Customers implements Serializable {

	private static final long serialVersionUID = 34131213L;

	@Id
	@GeneratedValue(strategy = GenerationType.AUTO)
	private Long id;

	@Column(name = "first_name", nullable = false)
	private String firstName;

	@Column(name = "last_name")
	private String lastName;

	@Column(name = "balance")
	private Long walletBalance;

	public static class CustomersBuilder {
		private Long id;
		private String firstName;
		private String lastName;
		private Long walletBalance;

		CustomersBuilder() {
		}

		public CustomersBuilder id(final Long id) {
			this.id = id;
			return this;
		}

		public CustomersBuilder firstName(final String firstName) {
			this.firstName = firstName;
			return this;
		}

		public CustomersBuilder lastName(final String lastName) {
			this.lastName = lastName;
			return this;
		}

		public CustomersBuilder walletBalance(final Long walletBalance) {
			this.walletBalance = walletBalance;
			return this;
		}

		public Customers build() {
			return new Customers(id, firstName, lastName, walletBalance);
		}

		public static CustomersBuilder builder() {
			return new CustomersBuilder();
		}

	}

	public static CustomersBuilder builder() {
		return new CustomersBuilder();
	}

	public Customers() {
	}

	public Customers(final Long id, final String firstName, final String lastName,
			final Long walletBalance) {
		this.id = id;
		this.firstName = firstName;
		this.lastName = lastName;
		this.walletBalance = walletBalance;
	}

	public boolean equals(final java.lang.Object o) {
		if (o == this)
			return true;
		if (!(o instanceof Customers))
			return false;
		final Customers other = (Customers) o;
		if (!other.canEqual((java.lang.Object) this))
			return false;
		final java.lang.Object this$id = this.getId();
		final java.lang.Object other$id = other.getId();
		if (this$id == null ? other$id != null : !this$id.equals(other$id))
			return false;
		final java.lang.Object this$firstName = this.getFirstName();
		final java.lang.Object other$firstName = other.getFirstName();
		if (this$firstName == null ? other$firstName != null
				: !this$firstName.equals(other$firstName))
			return false;
		final java.lang.Object this$lastName = this.getLastName();
		final java.lang.Object other$lastName = other.getLastName();
		if (this$lastName == null ? other$lastName != null
				: !this$lastName.equals(other$lastName))
			return false;
		final java.lang.Object this$walletBalance = this.getWalletBalance();
		final java.lang.Object other$walletBalance = other.getWalletBalance();
		if (this$walletBalance == null ? other$walletBalance != null
				: !this$walletBalance.equals(other$walletBalance))
			return false;
		return true;
	}

	protected boolean canEqual(final java.lang.Object other) {
		return other instanceof Customers;
	}

	public int hashCode() {
		final int PRIME = 59;
		int result = 1;
		final java.lang.Object $id = this.getId();
		result = result * PRIME + ($id == null ? 43 : $id.hashCode());
		final java.lang.Object $firstName = this.getFirstName();
		result = result * PRIME + ($firstName == null ? 43 : $firstName.hashCode());
		final java.lang.Object $lastName = this.getLastName();
		result = result * PRIME + ($lastName == null ? 43 : $lastName.hashCode());
		final java.lang.Object $walletBalance = this.getWalletBalance();
		result = result * PRIME
				+ ($walletBalance == null ? 43 : $walletBalance.hashCode());
		return result;
	}

	public java.lang.String toString() {
		return "Customers(id=" + this.getId() + ", firstName=" + this.getFirstName()
				+ ", lastName=" + this.getLastName() + ", walletBalance="
				+ this.getWalletBalance() + ")";
	}

	public Long getId() {
		return this.id;
	}

	public String getFirstName() {
		return this.firstName;
	}

	public String getLastName() {
		return this.lastName;
	}

	public Long getWalletBalance() {
		return this.walletBalance;
	}

	public void setId(final Long id) {
		this.id = id;
	}

	public void setFirstName(final String firstName) {
		this.firstName = firstName;
	}

	public void setLastName(final String lastName) {
		this.lastName = lastName;
	}

	public void setWalletBalance(final Long walletBalance) {
		this.walletBalance = walletBalance;
	}

}
