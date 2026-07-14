package example.service;

import org.springframework.stereotype.Service;
import org.springframework.web.util.UriComponentsBuilder;

import example.config.ProviderProperties;
import example.model.ProxyRequest;

import java.net.URI;
import java.util.List;

@Service
public class UrlBuilderService {
    public URI build(ProxyRequest request) {
        ProviderProperties provider = request.provider();
        UriComponentsBuilder builder = UriComponentsBuilder.fromHttpUrl(provider.getBaseUrl()).path(request.path());
        request.query().forEach((String name,  List<String> values) -> values.forEach(value -> builder.queryParam(name, value)));
        return builder.build().encode().toUri();
    }
}