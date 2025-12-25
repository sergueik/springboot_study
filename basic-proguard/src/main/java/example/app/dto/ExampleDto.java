package example.app.dto;

import lombok.Builder;
import lombok.Data;

/**
 * Created by seregaSLM on 07.07.2017.
 */
@Data
@Builder
public class ExampleDto {
    private String variableString;
    private int variableInt;
    private Object variableObject;
}