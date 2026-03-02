
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

RUN ["addgroup", "--gid", "${APP_GID}", "appgroup"]
RUN ["adduser", "--disabled-password", "--gecos", "", "--uid", "${APP_UID}", "--gid", "${APP_GID}", "appuser"]

COPY --chown=appuser:appgroup --from=build /app/target/example.distroless.jar app.jar

# NOTE: the chown is absent in distroless image, so the following will fail:
# RUN ["chown", "-R", "appuser:appgroup", "/app"]
EXPOSE 8080

USER appuser:appgroup

ENTRYPOINT ["java", "-jar", "/app/app.jar"]
