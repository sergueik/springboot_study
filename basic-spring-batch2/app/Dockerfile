FROM eclipse-temurin:11-jre-alpine
ARG app_jar="example.springboot-batch.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ENTRYPOINT ["sh", "-c", "\
until nc -z mysql-server 3306; do \
  echo waiting for mysql; \
  sleep 5; \
done; \
java -jar /app.jar"]
