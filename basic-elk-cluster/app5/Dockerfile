FROM openjdk:8-jre-alpine3.9
ARG HOME='/home'
RUN apk add curl 
WORKDIR /home
ARG ELASTIC_APM_AGENT_VERSION="1.35.0" 
ARG APM_SERVER="apm_server"
RUN wget -O ${HOME}/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar
ARG app_jar="example.soap-service.jar"
ADD "target/${app_jar}" ${HOME}/app.jar
ADD src/main/resources src/main/resources
EXPOSE 8888
ENV APM_SERVER=${APM_SERVER}
# NOTE:  env expansion issue:
# APM agent: Failed to handle event of type TRANSACTION with this error: ${APM_SERVER}

# ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Delastic.apm.service_name=java-soap-service", "-Delastic.apm.application_packages=example", "-Delastic.apm.server_url=http://${APM_SERVER}:8200", "-Djava.security.egd=file:/dev/./urandom", "-cp", "/home/app.jar", "example.Application"]

ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Delastic.apm.service_name=java-soap-service", "-Delastic.apm.application_packages=example", "-Delastic.apm.server_url=http://apm-server:8200", "-Djava.security.egd=file:/dev/./urandom", "-cp", "/home/app.jar", "example.Application"]
