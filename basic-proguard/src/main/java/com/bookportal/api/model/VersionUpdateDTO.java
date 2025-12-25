package com.bookportal.api.model;

import com.bookportal.api.entity.Version;
import com.bookportal.api.model.enums.EnvironmentEnum;
import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class VersionUpdateDTO {
    private String marketVersion;
    private String minVersion;
    private EnvironmentEnum environment;

    public static VersionUpdateDTO versionToUpdateResponse(Version version) {
        return VersionUpdateDTO.builder()
                .environment(version.getEnvironment())
                .marketVersion(version.getMarketVersion())
                .minVersion(version.getMinVersion())
                .build();
    }
}
