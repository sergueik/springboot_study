FROM openjdk:8-jre-alpine3.9
# FROM openjdk:8-jdk-alpine
VOLUME /tmp
ARG JAR_FILE="target/example.basic.jar"
ARG ELASTIC_APM_AGEN_VERSION=0.7.0
RUN wget https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGEN_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGEN_VERSION}.jar \
    -O ${HOME}/elastic-apm-agent.jar
COPY ${JAR_FILE} app.jar
ENTRYPOINT [ "java", "-javaagent:${HOME}/elastic-apm-agent.jar", "-Djava.security.egd=file:/dev/./urandom", "-jar", "/app.jar" ]
