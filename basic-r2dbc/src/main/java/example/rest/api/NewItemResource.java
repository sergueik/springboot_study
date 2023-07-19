package example.rest.api;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.Size;
import java.util.Set;

public class NewItemResource {
	@NotBlank
	@Size(max = 4000)
	private String description;
	private Long assigneeId;
	private Set<Long> tagIds;

	@java.lang.SuppressWarnings("all")
	public NewItemResource() {
	}

	@java.lang.SuppressWarnings("all")
	public String getDescription() {
		return this.description;
	}

	@java.lang.SuppressWarnings("all")
	public Long getAssigneeId() {
		return this.assigneeId;
	}

	@java.lang.SuppressWarnings("all")
	public Set<Long> getTagIds() {
		return this.tagIds;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public NewItemResource setDescription(final String description) {
		this.description = description;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public NewItemResource setAssigneeId(final Long assigneeId) {
		this.assigneeId = assigneeId;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public NewItemResource setTagIds(final Set<Long> tagIds) {
		this.tagIds = tagIds;
		return this;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public boolean equals(final java.lang.Object o) {
		if (o == this)
			return true;
		if (!(o instanceof NewItemResource))
			return false;
		final NewItemResource other = (NewItemResource) o;
		if (!other.canEqual((java.lang.Object) this))
			return false;
		final java.lang.Object this$assigneeId = this.getAssigneeId();
		final java.lang.Object other$assigneeId = other.getAssigneeId();
		if (this$assigneeId == null ? other$assigneeId != null
				: !this$assigneeId.equals(other$assigneeId))
			return false;
		final java.lang.Object this$description = this.getDescription();
		final java.lang.Object other$description = other.getDescription();
		if (this$description == null ? other$description != null
				: !this$description.equals(other$description))
			return false;
		final java.lang.Object this$tagIds = this.getTagIds();
		final java.lang.Object other$tagIds = other.getTagIds();
		if (this$tagIds == null ? other$tagIds != null
				: !this$tagIds.equals(other$tagIds))
			return false;
		return true;
	}

	@java.lang.SuppressWarnings("all")
	protected boolean canEqual(final java.lang.Object other) {
		return other instanceof NewItemResource;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public int hashCode() {
		final int PRIME = 59;
		int result = 1;
		final java.lang.Object $assigneeId = this.getAssigneeId();
		result = result * PRIME
				+ ($assigneeId == null ? 43 : $assigneeId.hashCode());
		final java.lang.Object $description = this.getDescription();
		result = result * PRIME
				+ ($description == null ? 43 : $description.hashCode());
		final java.lang.Object $tagIds = this.getTagIds();
		result = result * PRIME + ($tagIds == null ? 43 : $tagIds.hashCode());
		return result;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "NewItemResource(description=" + this.getDescription()
				+ ", assigneeId=" + this.getAssigneeId() + ", tagIds="
				+ this.getTagIds() + ")";
	}
}
