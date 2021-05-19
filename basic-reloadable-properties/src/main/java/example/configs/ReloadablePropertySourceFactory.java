package example.configs;

import java.io.IOException;

import org.springframework.core.env.PropertySource;
import org.springframework.core.io.Resource;
import org.springframework.core.io.support.DefaultPropertySourceFactory;
import org.springframework.core.io.support.EncodedResource;

public class ReloadablePropertySourceFactory
		extends DefaultPropertySourceFactory {
	@Override
	public PropertySource<?> createPropertySource(String s,
			EncodedResource encodedResource) throws IOException {
		Resource internal = encodedResource.getResource();
		return super.createPropertySource(s, encodedResource);
	}
}
