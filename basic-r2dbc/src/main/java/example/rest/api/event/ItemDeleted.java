package example.rest.api.event;

import lombok.Value;

@Value
public class ItemDeleted implements Event {

    Long itemId;

}
