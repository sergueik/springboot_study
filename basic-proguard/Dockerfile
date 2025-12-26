FROM eclipse-temurin:11-jdk-alpine as builder
ARG app_jar=com.bookportal.api.jar
RUN apk upgrade -U -a \
    && apk add --no-cache \
    maven \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk
WORKDIR app
ADD src src/
COPY pom.xml proguard.conf proguard.txt ./
RUN mvn -ntp dependency:go-offline
RUN mvn -T 2C -Dmaven.test.skip=true clean package
# comment the following layers is there is an obfuscation error
RUN mvn -T 2C -Pproguard clean package

FROM eclipse-temurin:11-jre-alpine
# NOTE: need to re-introduce ARG
ARG app_jar=example.upload.jar
WORKDIR app
COPY --from=builder "/app/target/${app_jar}" /app/app.jar
EXPOSE 8085
ENTRYPOINT ["java", "-jar", "app.jar"]
