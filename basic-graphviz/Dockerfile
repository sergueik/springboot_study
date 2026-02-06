FROM eclipse-temurin:11-jre-alpine
WORKDIR /app
ARG app_jar="example.graphviz-java.jar"
ADD "target/${app_jar}" app.jar
ADD target/lib lib/
RUN rm -f lib/j2v8*
ENTRYPOINT [ "java", "-cp", "/app/app.jar:/app/lib/*", "example.Example" ]
