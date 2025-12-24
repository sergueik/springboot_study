FROM maven:3.6.3-adoptopenjdk-11
WORKDIR  /usr/src/proguard-spring-boot-example
ADD . /usr/src/proguard-spring-boot-example
RUN mvn -T 2C -Djava.version=11 clean install
CMD java -jar ./target/spring.boot.jar
