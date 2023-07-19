package example.rest.api;

import java.time.LocalDateTime;
import java.util.List;
import example.model.ItemStatus;
import example.model.Tag;

public class ItemResource {
	private Long id;
	private Long version;
	private String description;
	private ItemStatus status;
	private PersonResource assignee;
	private List<Tag> tags;
	private LocalDateTime createdDate;
	private LocalDateTime lastModifiedDate;

	@java.lang.SuppressWarnings("all")
	public ItemResource() {
	}

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
	public PersonResource getAssignee() {
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
	public ItemResource setId(final Long id) {
		this.id = id;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemResource setVersion(final Long version) {
		this.version = version;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemResource setDescription(final String description) {
		this.description = description;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemResource setStatus(final ItemStatus status) {
		this.status = status;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemResource setAssignee(final PersonResource assignee) {
		this.assignee = assignee;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemResource setTags(final List<Tag> tags) {
		this.tags = tags;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemResource setCreatedDate(final LocalDateTime createdDate) {
		this.createdDate = createdDate;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemResource setLastModifiedDate(
			final LocalDateTime lastModifiedDate) {
		this.lastModifiedDate = lastModifiedDate;
		return this;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public boolean equals(final java.lang.Object o) {
		if (o == this)
			return true;
		if (!(o instanceof ItemResource))
			return false;
		final ItemResource other = (ItemResource) o;
		if (!other.canEqual((java.lang.Object) this))
			return false;
		final java.lang.Object this$id = this.getId();
		final java.lang.Object other$id = other.getId();
		if (this$id == null ? other$id != null : !this$id.equals(other$id))
			return false;
		final java.lang.Object this$version = this.getVersion();
		final java.lang.Object other$version = other.getVersion();
		if (this$version == null ? other$version != null
				: !this$version.equals(other$version))
			return false;
		final java.lang.Object this$description = this.getDescription();
		final java.lang.Object other$description = other.getDescription();
		if (this$description == null ? other$description != null
				: !this$description.equals(other$description))
			return false;
		final java.lang.Object this$status = this.getStatus();
		final java.lang.Object other$status = other.getStatus();
		if (this$status == null ? other$status != null
				: !this$status.equals(other$status))
			return false;
		final java.lang.Object this$assignee = this.getAssignee();
		final java.lang.Object other$assignee = other.getAssignee();
		if (this$assignee == null ? other$assignee != null
				: !this$assignee.equals(other$assignee))
			return false;
		final java.lang.Object this$tags = this.getTags();
		final java.lang.Object other$tags = other.getTags();
		if (this$tags == null ? other$tags != null : !this$tags.equals(other$tags))
			return false;
		final java.lang.Object this$createdDate = this.getCreatedDate();
		final java.lang.Object other$createdDate = other.getCreatedDate();
		if (this$createdDate == null ? other$createdDate != null
				: !this$createdDate.equals(other$createdDate))
			return false;
		final java.lang.Object this$lastModifiedDate = this.getLastModifiedDate();
		final java.lang.Object other$lastModifiedDate = other.getLastModifiedDate();
		if (this$lastModifiedDate == null ? other$lastModifiedDate != null
				: !this$lastModifiedDate.equals(other$lastModifiedDate))
			return false;
		return true;
	}

	@java.lang.SuppressWarnings("all")
	protected boolean canEqual(final java.lang.Object other) {
		return other instanceof ItemResource;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public int hashCode() {
		final int PRIME = 59;
		int result = 1;
		final java.lang.Object $id = this.getId();
		result = result * PRIME + ($id == null ? 43 : $id.hashCode());
		final java.lang.Object $version = this.getVersion();
		result = result * PRIME + ($version == null ? 43 : $version.hashCode());
		final java.lang.Object $description = this.getDescription();
		result = result * PRIME
				+ ($description == null ? 43 : $description.hashCode());
		final java.lang.Object $status = this.getStatus();
		result = result * PRIME + ($status == null ? 43 : $status.hashCode());
		final java.lang.Object $assignee = this.getAssignee();
		result = result * PRIME + ($assignee == null ? 43 : $assignee.hashCode());
		final java.lang.Object $tags = this.getTags();
		result = result * PRIME + ($tags == null ? 43 : $tags.hashCode());
		final java.lang.Object $createdDate = this.getCreatedDate();
		result = result * PRIME
				+ ($createdDate == null ? 43 : $createdDate.hashCode());
		final java.lang.Object $lastModifiedDate = this.getLastModifiedDate();
		result = result * PRIME
				+ ($lastModifiedDate == null ? 43 : $lastModifiedDate.hashCode());
		return result;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "ItemResource(id=" + this.getId() + ", version=" + this.getVersion()
				+ ", description=" + this.getDescription() + ", status="
				+ this.getStatus() + ", assignee=" + this.getAssignee() + ", tags="
				+ this.getTags() + ", createdDate=" + this.getCreatedDate()
				+ ", lastModifiedDate=" + this.getLastModifiedDate() + ")";
	}
}
