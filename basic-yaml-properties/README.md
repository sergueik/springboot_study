
### Info

This directory contains moderately modified Springboot yaml properties file support [project code repository](https://github.com/mkyong/spring-boot/tree/master/yaml-simple)

See also the original [documentation](https://mkyong.com/spring-boot/spring-boot-yaml-example/)

### Note

With old version __1.5.4.RELEASE__ Springboot  the following configuration 
```yaml
spring:
  profiles:
    active: dev
  main:
    banner-mode: false
  # banner-mode: off
```  
lead to runtime exception:
```sh
Failed to convert property value of type 'java.lang.Boolean' to required type 'org.springframework.boot.Banner$Mode' for property 'bannerMode'; nested exception is java.lang.IllegalStateException: 

Cannot convert value of type 'java.lang.Boolean' to required type 'org.springframework.boot.Banner$Mode' for property 'bannerMode': no matching editors or conversion strategy found
```
both `false` or `off`  lead to the error. The newer versions of  parent project do not have this issue


### Author

[Serguei Kouzmine](kouzmine_serguei@yahoo.com)

