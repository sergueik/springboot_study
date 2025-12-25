package com.bookportal.api.service;

import com.bookportal.api.configs.EnvironmentVariables;
import com.bookportal.api.model.VersionResponseDTO;
import com.bookportal.api.model.VersionUpdateDTO;
import com.bookportal.api.model.enums.EnvironmentEnum;
import com.bookportal.api.repository.VersionRepository;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Propagation;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.server.ResponseStatusException;
import reactor.core.publisher.Mono;

@Service
@RequiredArgsConstructor
public class VersionService {
    private final VersionRepository versionRepository;
    private final EnvironmentVariables environmentVariables;

    public Mono<VersionResponseDTO> checkVersion(String version, String environment) {
        if (EnvironmentEnum.isExist(environment)) {
            return versionRepository.findByEnvironment(EnvironmentEnum.findByValue(environment))
                    .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, environmentVariables.environmentNotFound())))
                    .map(version1 -> {
                        String minVersion = version1.getMinVersion();
                        EnvironmentEnum envValue = version1.getEnvironment();
                        String marketVersion = version1.getMarketVersion();

                        int userVersionNumber = Integer.parseInt(version.replaceAll("\\.", ""));
                        int minVersionNumber = Integer.parseInt(minVersion.replaceAll("\\.", ""));
                        int marketVersionNumber = Integer.parseInt(marketVersion.replaceAll("\\.", ""));

                        boolean force = userVersionNumber < minVersionNumber;
                        boolean should = userVersionNumber < marketVersionNumber;

                        return VersionResponseDTO.builder()
                                .environment(envValue)
                                .minVersion(minVersion)
                                .marketVersion(marketVersion)
                                .force(force)
                                .should(should)
                                .build();
                    });
        }
        throw new ResponseStatusException(HttpStatus.BAD_REQUEST, environmentVariables.invalidEnvironment());
    }

    @Transactional(propagation = Propagation.MANDATORY, rollbackFor = Exception.class)
    public Mono<VersionUpdateDTO> updateVersion(String minVersion, String marketVersion, String environment) {
        versionValidation(minVersion);
        versionValidation(marketVersion);

        if (EnvironmentEnum.isExist(environment)) {
            return versionRepository.findByEnvironment(EnvironmentEnum.findByValue(environment))
                    .switchIfEmpty(Mono.error(new ResponseStatusException(HttpStatus.BAD_REQUEST, environmentVariables.environmentNotFound())))
                    .map(version -> {
                        version.setMinVersion(minVersion);
                        version.setMarketVersion(marketVersion);
                        return version;
                    })
                    .flatMap(versionRepository::save)
                    .map(VersionUpdateDTO::versionToUpdateResponse);
        }
        throw new ResponseStatusException(HttpStatus.BAD_REQUEST, environmentVariables.invalidEnvironment());
    }

    private void versionValidation(String version) {
        int count = 0;
        version = version.trim();

        if (version.length() == 0) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, environmentVariables.invalidVersion());
        }

        // first char should be a number
        if (!isNumeric(String.valueOf(version.charAt(0)))) {
            count++;
        }
        // last char should be a number
        if (!isNumeric(String.valueOf(version.charAt(version.length() - 1)))) {
            count++;
        }

        String[] split = version.split("\\.");
        if (split.length != 3) {
            count++;
        }

        for (String v : split) {
            boolean numeric = isNumeric(v);
            if (!numeric) count++;
        }
        if (count > 0) {
            throw new ResponseStatusException(HttpStatus.BAD_REQUEST, environmentVariables.invalidVersion());
        }
    }

    private boolean isNumeric(String str) {
        for (char c : str.toCharArray()) {
            if (!Character.isDigit(c)) return false;
        }
        return true;
    }

}
