FROM eclipse-temurin:11-jdk as builder
WORKDIR /app
COPY target/app.jar app.jar
RUN java -Djarmode=layertools -jar app.jar extract

FROM eclipse-temurin:11-jre
WORKDIR /app
COPY --from=builder dependencies/ ./
COPY --from=builder spring-boot-loader/ ./
COPY --from=builder snapshot-dependencies/ ./
COPY --from=builder application/ ./

ENTRYPOINT ["java","org.springframework.boot.loader.launch.JarLauncher"]

