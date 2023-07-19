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
public class Tag {
	@Id
	private Long id;
	@Version
	private Long version;
	@NotBlank
	@Size(max = 100)
	private String name;
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
	public String getName() {
		return this.name;
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
	public Tag setId(final Long id) {
		this.id = id;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Tag setVersion(final Long version) {
		this.version = version;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Tag setName(final String name) {
		this.name = name;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Tag setCreatedDate(final LocalDateTime createdDate) {
		this.createdDate = createdDate;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Tag setLastModifiedDate(final LocalDateTime lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
		return this;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "Tag(id=" + this.getId() + ", version=" + this.getVersion()
				+ ", name=" + this.getName() + ", createdDate=" + this.getCreatedDate()
				+ ", lastModifiedDate=" + this.getLastModifiedDate() + ")";
	}
}
