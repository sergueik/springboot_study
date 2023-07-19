package example.rest.api;

import lombok.Data;
import lombok.experimental.Accessors;

import javax.validation.constraints.NotBlank;
import javax.validation.constraints.NotNull;
import javax.validation.constraints.Size;

import example.model.ItemStatus;

import java.util.Set;

@Data
@Accessors(chain = true)
public class ItemUpdateResource {

    @NotBlank
    @Size(max=4000)
    private String description;

    @NotNull
    private ItemStatus status;

    private Long assigneeId;

    private Set<Long> tagIds;
}
