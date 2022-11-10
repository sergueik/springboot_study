package example.model.grafana;

import com.fasterxml.jackson.annotation.JsonProperty;

public class Time{
  public String from;
  @JsonProperty("to") 
  public String myto;
}
