package com.bookportal.api.model;

import com.bookportal.api.model.enums.EnvironmentEnum;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class VersionResponseDTO {
    private EnvironmentEnum environment;
    private String minVersion;
    private String marketVersion;
    private boolean force;
    private boolean should;

}
