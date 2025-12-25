FROM maven:3.6.3-adoptopenjdk-11
WORKDIR app
ADD src app
COPY pom.xml app
RUN mvn -T 2C -Djava.version=11 -Pproguard clean install
CMD java -jar ./target/spring.boot.jar
