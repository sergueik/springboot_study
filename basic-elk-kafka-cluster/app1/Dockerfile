FROM eclipse-temurin:11-jre-alpine

ARG HOME=/home
WORKDIR ${HOME}
RUN apk add --no-cache curl wget

ARG ELASTIC_APM_AGENT_VERSION=1.49.0
ARG APM_SERVER="apm_server"
RUN wget -O ${HOME}/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar
ARG app_jar="example.app1.jar"
ADD "target/${app_jar}" ${HOME}/app.jar

EXPOSE 8080
ENV APM_SERVER=${APM_SERVER}

ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Delastic.apm.service_name=app1", "-Delastic.apm.application_packages=example", "-Delastic.apm.server_url=http://apm-server:8200", "-Djava.security.egd=file:/dev/./urandom", "-jar", "/home/app.jar" ]
