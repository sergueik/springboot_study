FROM openjdk:8-jre-alpine3.9
COPY EchoServer.class EchoServer.class
# NO javac in openjdk-jre image
# COPY EchoServer.java EchoServer.java
# RUN javac EchoServer.java
ARG PORT=8889
ENV PORT=$PORT 
EXPOSE $PORT
ENTRYPOINT ["java", "EchoServer"]
