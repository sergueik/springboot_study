### Info

This directoty contains `Dockerfile`, `spec_helper.rb` and tests laid based on 
[docker-image-testing-example](https://github.com/chorrell/docker-image-testing-example) example project, bundling together the build of image and container with scheduling the serverspec on the same. The original projects does not parameterize well.



### Note

to workaround the 
```sh
Docker::Error::TimeoutError:
       read timeout reached
```
error, pre-build the image ahead of time


### See Also:

  * https://github.com/mnishikizawa/docker-serverspec/blob/master/spec/Dockerfile_spec.rb
