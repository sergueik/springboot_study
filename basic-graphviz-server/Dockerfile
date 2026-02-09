FROM eclipse-temurin:17-jre-jammy

RUN apt-get update \
    && apt-get install -y --no-install-recommends curl \
    && rm -rf /var/lib/apt/lists/*

WORKDIR /app
ARG app_jar="example.graphviz-java.jar"
ADD "target/${app_jar}" app.jar
ADD "target/lib" lib/

HEALTHCHECK --interval=10s --timeout=3s --start-period=60s --retries=5 \
  CMD curl -f http://localhost:8080/health || exit 1
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -cp /app/app.jar:/app/lib/* example.DotGraphics 8080"]
