FROM maven:3.9.5-eclipse-temurin-17-alpine as builder

WORKDIR /app

COPY pom.xml ./
RUN mvn dependency:go-offline -q

# TODO:
# RUN gradle clean bootJar --no-daemon

ADD src ./src/
RUN mvn clean package -DskipTests -q

FROM eclipse-temurin:17-jre-alpine as run

WORKDIR /app

COPY --from=builder /app/target/ai-proxy-*.jar app.jar

ENV PORT=8080
EXPOSE 8080

HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
CMD curl -f http://localhost:8080/actuator/health || exit 1

ENTRYPOINT ["java", "-jar", "/app/app.jar"]
