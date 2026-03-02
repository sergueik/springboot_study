
FROM maven:3.9.3-eclipse-temurin-11-alpine AS build
WORKDIR /app

COPY pom.xml .
RUN mvn dependency:go-offline -B

COPY src ./src
RUN mvn clean package -DskipTests -B

# Optional: tiny Alpine intermediate runtime

FROM gcr.io/distroless/java11-debian11

WORKDIR /app

# Load UID/GID from the build args to create a user inline
ARG APP_UID=1000
ARG APP_GID=1000

COPY --chown=${APP_UID}:${APP_GID} --from=build /app/target/example.distroless.jar app.jar

EXPOSE 8080

USER ${APP_UID}:${APP_GID}

ENTRYPOINT ["java", "-jar", "/app/app.jar"]
