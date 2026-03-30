FROM eclipse-temurin:11-jre-alpine

ARG HOME=/home
WORKDIR ${HOME}
RUN apk add --no-cache curl wget

# FROM eclipse-temurin:11-jre-jammy
# With eclipse-temurin:11-jre
# the underlying Ubuntu “noble” (23.04) repositories are broken or removed/expired and one cannot RUN apt-get update - the signatures couldn't be verified because the public key is not available: NO_PUBKEY 871920D1991BC93C

ARG ELASTIC_APM_AGENT_VERSION=1.49.0
ARG APM_SERVER="apm_server"
RUN wget -O ${HOME}/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar
ARG app_jar="example.app2.jar"
ADD "target/${app_jar}" ${HOME}/app.jar

ENV APM_SERVER=${APM_SERVER}

ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Delastic.apm.service_name=app2", "-Delastic.apm.application_packages=example", "-Delastic.apm.server_url=http://apm-server:8200", "-Djava.security.egd=file:/dev/./urandom", "-jar", "/home/app.jar" ]
