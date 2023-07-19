package example.rest.api.event;

public final class ItemDeleted implements Event {
	private final Long itemId;

	@java.lang.SuppressWarnings("all")
	public ItemDeleted(final Long itemId) {
		this.itemId = itemId;
	}

	@java.lang.SuppressWarnings("all")
	public Long getItemId() {
		return this.itemId;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public boolean equals(final java.lang.Object o) {
		if (o == this)
			return true;
		if (!(o instanceof ItemDeleted))
			return false;
		final ItemDeleted other = (ItemDeleted) o;
		final java.lang.Object this$itemId = this.getItemId();
		final java.lang.Object other$itemId = other.getItemId();
		if (this$itemId == null ? other$itemId != null
				: !this$itemId.equals(other$itemId))
			return false;
		return true;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public int hashCode() {
		final int PRIME = 59;
		int result = 1;
		final java.lang.Object $itemId = this.getItemId();
		result = result * PRIME + ($itemId == null ? 43 : $itemId.hashCode());
		return result;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "ItemDeleted(itemId=" + this.getItemId() + ")";
	}
}
