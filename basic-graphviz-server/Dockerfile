FROM eclipse-temurin:11-jre-alpine
# RUN apk add --no-cache --virtual .dummy graphviz && apk del .dummy
RUN apk add --no-cache graphviz curl \
   && rm -rf /var/cache/apk \
   && mkdir /var/cache/apk
WORKDIR /app
ARG app_jar="example.graphviz-java-fat.jar"
ADD "target/${app_jar}" app.jar
HEALTHCHECK --interval=10s --timeout=3s --start-period=60s --retries=5 \
  CMD curl -f http://localhost:8080/health || exit 1
ENTRYPOINT ["sh", "-c", "java $JAVA_OPTS -jar /app/app.jar 8080"]
