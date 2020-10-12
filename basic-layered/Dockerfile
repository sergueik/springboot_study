FROM alpine-java11-maven as builder
# first stage build the package with layers
# NOTE: we have not specified Java 11 in the pom.xml
RUN mkdir application
COPY src application/src
COPY pom.xml application
RUN mvn -f /application/pom.xml package
WORKDIR application

# FROM openjdk:8-jdk-alpine3.9 as extractor
FROM amond/openjdk:11-jdk-alpine as extractor
# second stage extract the layers
WORKDIR application
ARG app_jar="example.basic.jar"
# COPY --from=builder /application/target/${app.jar} ./app.jar
# NOTE: cannot use ARG in the copy command:
# failed to process "application/target/${app.jar}": missing ':' in substitution
COPY --from=builder /application/target/example.basic.jar ./app.jar
RUN java -Djarmode=layertools -jar app.jar extract
# alternatively
# COPY --from=builder /application/target/ .
# RUN java -Djarmode=layertools -jar ${app.jar} extract


# third stage copy the extracted layers
# FROM openjdk:8-jre-alpine3.9
FROM amond/openjdk:11-jdk-alpine
WORKDIR application
COPY --from=extractor application/dependencies/ ./
COPY --from=extractor application/spring-boot-loader/ ./
COPY --from=extractor application/snapshot-dependencies/ ./
COPY --from=extractor application/application/ ./
# ENTRYPOINT "sh"
# NOTE: starting with the wrapper class, not application main class "example.Application"
# ENTRYPOINT ["java", "example.ExampleApplication"]
ENTRYPOINT ["java", "org.springframework.boot.loader.JarLauncher"]
