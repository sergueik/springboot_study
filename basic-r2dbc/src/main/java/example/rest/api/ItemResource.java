package example.rest.api;

import lombok.Data;
import lombok.experimental.Accessors;

import java.time.LocalDateTime;
import java.util.List;

import example.model.ItemStatus;
import example.model.Tag;

@Data
@Accessors(chain = true)
public class ItemResource {

    private Long id;
    private Long version;

    private String description;
    private ItemStatus status;

    private PersonResource assignee;
    private List<Tag> tags;

    private LocalDateTime createdDate;
    private LocalDateTime lastModifiedDate;

}
