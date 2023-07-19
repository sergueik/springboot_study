package example.model;

import org.springframework.data.annotation.CreatedDate;
import org.springframework.data.annotation.Id;
import org.springframework.data.annotation.LastModifiedDate;
import org.springframework.data.annotation.Version;
import org.springframework.data.relational.core.mapping.Table;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;

@Table
public class Person {
	@Id
	private Long id;
	@Version
	private Long version;
	@NotBlank
	@Size(max = 100)
	private String firstName;
	@NotBlank
	@Size(max = 100)
	private String lastName;
	@CreatedDate
	private LocalDateTime createdDate;
	@LastModifiedDate
	private LocalDateTime lastModifiedDate;

	@java.lang.SuppressWarnings("all")
	public Long getId() {
		return this.id;
	}

	@java.lang.SuppressWarnings("all")
	public Long getVersion() {
		return this.version;
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
	public LocalDateTime getCreatedDate() {
		return this.createdDate;
	}

	@java.lang.SuppressWarnings("all")
	public LocalDateTime getLastModifiedDate() {
		return this.lastModifiedDate;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Person setId(final Long id) {
		this.id = id;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Person setVersion(final Long version) {
		this.version = version;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Person setFirstName(final String firstName) {
		this.firstName = firstName;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Person setLastName(final String lastName) {
		this.lastName = lastName;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Person setCreatedDate(final LocalDateTime createdDate) {
		this.createdDate = createdDate;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Person setLastModifiedDate(final LocalDateTime lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
		return this;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "Person(id=" + this.getId() + ", version=" + this.getVersion()
				+ ", firstName=" + this.getFirstName() + ", lastName="
				+ this.getLastName() + ", createdDate=" + this.getCreatedDate()
				+ ", lastModifiedDate=" + this.getLastModifiedDate() + ")";
	}
}
