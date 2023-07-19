package example.mapper;

import example.model.Item;
import example.rest.api.ItemPatchResource;
import example.rest.api.ItemResource;
import example.rest.api.ItemUpdateResource;
import example.rest.api.NewItemResource;

import org.mapstruct.AfterMapping;
import org.mapstruct.Mapper;
import org.mapstruct.Mapping;
import org.mapstruct.MappingTarget;
import org.springframework.beans.factory.annotation.Autowired;

@Mapper(componentModel = "spring", uses = { PersonMapper.class,
		TagMapper.class })
public abstract class ItemMapper {

	@Autowired
	private TagMapper tagMapper;

	public abstract ItemResource toResource(Item item);

	@Mapping(target = "id", ignore = true)
	@Mapping(target = "status", ignore = true)
	@Mapping(target = "createdDate", ignore = true)
	@Mapping(target = "lastModifiedDate", ignore = true)
	@Mapping(target = "version", ignore = true)
	@Mapping(target = "assignee", ignore = true)
	@Mapping(target = "tags", ignore = true)
	public abstract Item toModel(NewItemResource itemResource);

	@Mapping(target = "id", ignore = true)
	@Mapping(target = "createdDate", ignore = true)
	@Mapping(target = "lastModifiedDate", ignore = true)
	@Mapping(target = "version", ignore = true)
	@Mapping(target = "assignee", ignore = true)
	@Mapping(target = "tags", ignore = true)
	public abstract Item update(ItemUpdateResource itemResource,
			@MappingTarget Item item);

	@AfterMapping
	public void afterMapping(NewItemResource itemResource,
			@MappingTarget Item item) {
		item.setTags(tagMapper.toTags(itemResource.getTagIds()));
	}

	@AfterMapping
	public void afterMapping(ItemUpdateResource itemResource,
			@MappingTarget Item item) {
		item.setTags(tagMapper.toTags(itemResource.getTagIds()));
	}

	@SuppressWarnings({ "OptionalAssignedToNull" })
	public Item patch(ItemPatchResource patchResource, Item item) {
		if (patchResource.getDescription() != null) {
			// The description has been provided in the patch
			item.setDescription(patchResource.getDescription().orElse(null));
		}

		if (patchResource.getStatus() != null) {
			// The status has been provided in the patch
			item.setStatus(patchResource.getStatus().orElse(null));
		}

		if (patchResource.getAssigneeId() != null) {
			item.setAssigneeId(patchResource.getAssigneeId().orElse(null));
		}

		if (patchResource.getTagIds() != null) {
			// Set tags objects containing only the ID
			item.setTags(tagMapper.toTags(patchResource.getTagIds().orElse(null)));
		}
		return item;
	}

}
