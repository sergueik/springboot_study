package example.model;

import org.springframework.data.annotation.*;
import org.springframework.data.relational.core.mapping.Table;
import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;
import java.time.LocalDateTime;
import java.util.List;

@Table
public class Item {
	public Item(Long id, Long version) {
		this.id = id;
		this.version = version;
	}

	@Id
	private Long id;
	@Version
	private Long version;
	@Size(max = 4000)
	@NotBlank
	private String description;
	@NotNull
	private ItemStatus status = ItemStatus.TODO;
	private Long assigneeId;
	@Transient
	private Person assignee;
	@Transient
	private List<Tag> tags;
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
	public String getDescription() {
		return this.description;
	}

	@java.lang.SuppressWarnings("all")
	public ItemStatus getStatus() {
		return this.status;
	}

	@java.lang.SuppressWarnings("all")
	public Long getAssigneeId() {
		return this.assigneeId;
	}

	@java.lang.SuppressWarnings("all")
	public Person getAssignee() {
		return this.assignee;
	}

	@java.lang.SuppressWarnings("all")
	public List<Tag> getTags() {
		return this.tags;
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
	public Item setId(final Long id) {
		this.id = id;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setVersion(final Long version) {
		this.version = version;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setDescription(final String description) {
		this.description = description;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setStatus(final ItemStatus status) {
		this.status = status;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setAssigneeId(final Long assigneeId) {
		this.assigneeId = assigneeId;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setAssignee(final Person assignee) {
		this.assignee = assignee;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setTags(final List<Tag> tags) {
		this.tags = tags;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setCreatedDate(final LocalDateTime createdDate) {
		this.createdDate = createdDate;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public Item setLastModifiedDate(final LocalDateTime lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
		return this;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "Item(id=" + this.getId() + ", version=" + this.getVersion()
				+ ", description=" + this.getDescription() + ", status="
				+ this.getStatus() + ", assigneeId=" + this.getAssigneeId()
				+ ", assignee=" + this.getAssignee() + ", tags=" + this.getTags()
				+ ", createdDate=" + this.getCreatedDate() + ", lastModifiedDate="
				+ this.getLastModifiedDate() + ")";
	}

	@java.lang.SuppressWarnings("all")
	public Item() {
	}
}
