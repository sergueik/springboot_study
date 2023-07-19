package example.rest.api;

public class PersonResource {
	private Long id;
	private String firstName;
	private String lastName;

	@java.lang.SuppressWarnings("all")
	public PersonResource() {
	}

	@java.lang.SuppressWarnings("all")
	public Long getId() {
		return this.id;
	}

	@java.lang.SuppressWarnings("all")
	public String getFirstName() {
		return this.firstName;
	}

	@java.lang.SuppressWarnings("all")
	public String getLastName() {
		return this.lastName;
	}

	@java.lang.SuppressWarnings("all")
	public void setId(final Long id) {
		this.id = id;
	}

	@java.lang.SuppressWarnings("all")
	public void setFirstName(final String firstName) {
		this.firstName = firstName;
	}

	@java.lang.SuppressWarnings("all")
	public void setLastName(final String lastName) {
		this.lastName = lastName;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public boolean equals(final java.lang.Object o) {
		if (o == this)
			return true;
		if (!(o instanceof PersonResource))
			return false;
		final PersonResource other = (PersonResource) o;
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
		return true;
	}

	@java.lang.SuppressWarnings("all")
	protected boolean canEqual(final java.lang.Object other) {
		return other instanceof PersonResource;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public int hashCode() {
		final int PRIME = 59;
		int result = 1;
		final java.lang.Object $id = this.getId();
		result = result * PRIME + ($id == null ? 43 : $id.hashCode());
		final java.lang.Object $firstName = this.getFirstName();
		result = result * PRIME + ($firstName == null ? 43 : $firstName.hashCode());
		final java.lang.Object $lastName = this.getLastName();
		result = result * PRIME + ($lastName == null ? 43 : $lastName.hashCode());
		return result;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "PersonResource(id=" + this.getId() + ", firstName="
				+ this.getFirstName() + ", lastName=" + this.getLastName() + ")";
	}
}
