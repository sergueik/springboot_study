FROM eclipse-temurin:11-jre-alpine
RUN apk add --no-cache --virtual .dummy graphviz && apk del .dummy
RUN apk add --no-cache graphviz curl \
   && rm -rf /var/cache/apk \
   && mkdir /var/cache/apk
WORKDIR /app
ARG app_jar="DotGraphics.jar"
ADD "dist/${app_jar}" app.jar
# NOTE: when using JSON array syntax, arrays must be comprised of strings only
ENTRYPOINT [ "java", "-jar", "/app/app.jar", "8080" ]
