FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.basic-args.jar"
ADD "target/${app_jar}" app.jar
EXPOSE 8080
ENTRYPOINT ["java", "-Dparams=eyJuYW1lIjoibXkgZG9ja2VyaXplZCBocGFyYW1ldGVyaXplZCBzcHJpbmcgYXBwbGljYXRpb24iLCAic3VjY2VzcyI6dHJ1ZSwicmVzdWx0Ijo0MiwiaWQiOjAgfQo=",  "-jar", "app.jar" ]
