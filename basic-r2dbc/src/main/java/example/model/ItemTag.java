package example.model;

// import lombok.*;
import org.springframework.data.annotation.Id;
import org.springframework.data.relational.core.mapping.Table;
import javax.validation.constraints.NotNull;

@Table
public class ItemTag {
	public ItemTag(Long itemId, Long tagId) {
		this.itemId = itemId;
		this.tagId = tagId;
	}

	@Id
	private Long id;
	@NotNull
	private Long itemId;
	@NotNull
	private Long tagId;

	@java.lang.SuppressWarnings("all")
	public Long getId() {
		return this.id;
	}

	@java.lang.SuppressWarnings("all")
	public Long getItemId() {
		return this.itemId;
	}

	@java.lang.SuppressWarnings("all")
	public Long getTagId() {
		return this.tagId;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemTag setId(final Long id) {
		this.id = id;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemTag setItemId(final Long itemId) {
		this.itemId = itemId;
		return this;
	}

	/**
	 * @return {@code this}.
	 */
	@java.lang.SuppressWarnings("all")
	public ItemTag setTagId(final Long tagId) {
		this.tagId = tagId;
		return this;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "ItemTag(id=" + this.getId() + ", itemId=" + this.getItemId()
				+ ", tagId=" + this.getTagId() + ")";
	}

	@java.lang.SuppressWarnings("all")
	public ItemTag(final Long id, final Long itemId, final Long tagId) {
		this.id = id;
		this.itemId = itemId;
		this.tagId = tagId;
	}

	@java.lang.SuppressWarnings("all")
	public ItemTag() {
	}
}
