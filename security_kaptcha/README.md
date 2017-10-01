### Info

This directory contains a basic springboot authenticateFilter with kapcha based on
[wefine/springboot-security-kaptcha](https://github.com/wefine/springboot-security-kaptcha)

### Run application

Compile and start appplication
```sh
mvn clean spring-boot:run
```
Access application in browser: `http://localhost:8080/login`. 
Credentials are in `src/main/java/example/secure/kaptcha/config/SecurityConfiguration.java`, username: `user`, password: `password`
