package example.rest.api.event;

import example.rest.api.ItemResource;
import lombok.Data;
import lombok.Value;
import lombok.experimental.Accessors;

@Value
public class ItemSaved implements Event {

    ItemResource item;

}
