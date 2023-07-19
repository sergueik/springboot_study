package example.rest.api.event;

import example.rest.api.ItemResource;
// import lombok.Data;
// import lombok.experimental.Accessors;

public final class ItemSaved implements Event {
	private final ItemResource item;

	@java.lang.SuppressWarnings("all")
	public ItemSaved(final ItemResource item) {
		this.item = item;
	}

	@java.lang.SuppressWarnings("all")
	public ItemResource getItem() {
		return this.item;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public boolean equals(final java.lang.Object o) {
		if (o == this)
			return true;
		if (!(o instanceof ItemSaved))
			return false;
		final ItemSaved other = (ItemSaved) o;
		final java.lang.Object this$item = this.getItem();
		final java.lang.Object other$item = other.getItem();
		if (this$item == null ? other$item != null : !this$item.equals(other$item))
			return false;
		return true;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public int hashCode() {
		final int PRIME = 59;
		int result = 1;
		final java.lang.Object $item = this.getItem();
		result = result * PRIME + ($item == null ? 43 : $item.hashCode());
		return result;
	}

	@java.lang.Override
	@java.lang.SuppressWarnings("all")
	public java.lang.String toString() {
		return "ItemSaved(item=" + this.getItem() + ")";
	}
}
