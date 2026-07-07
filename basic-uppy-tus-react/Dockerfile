FROM maven:3.9.5-eclipse-temurin-11-alpine as builder

WORKDIR /app

COPY pom.xml ./
RUN mvn dependency:go-offline -q
ADD src ./src/

RUN mvn clean package -DskipTests -q 
# RUN mvn clean package -DskipTests -q  -o
# 
FROM eclipse-temurin:11-jre-alpine as run
COPY --from=builder /app/target/example.tus-java-server.jar /app/app.jar 
COPY frontend/.env ./

RUN apk update \
    && apk add --update --no-cache curl \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk

WORKDIR /app

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
CMD curl -f http://localhost:8080/ || exit 1

ENTRYPOINT ["java", "-jar", "/app/app.jar"]

EXPOSE 8080
