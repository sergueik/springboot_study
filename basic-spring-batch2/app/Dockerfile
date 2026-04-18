FROM eclipse-temurin:11-jre-alpine
ARG app_jar="example.springboot-batch.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
COPY wait-for-service.sh /wait-for-service.sh
RUN sed -i 's|\r$||g' /wait-for-service.sh && \
    chmod +x /wait-for-service.sh

ENTRYPOINT ["/wait-for-service.sh"]
CMD ["java", "-jar", "app.jar"]

