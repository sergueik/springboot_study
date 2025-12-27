FROM eclipse-temurin:11-jre-alpine
ARG HOME='/home'
RUN apk add curl netcat-openbsd \
  && rm -vrf /var/cache/apk/*
WORKDIR /home
ARG ELASTIC_APM_AGENT_VERSION="1.35.0" 
ARG APM_SERVER="apm_server"
RUN wget -O ${HOME}/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar
ARG app_jar="example.server.jar"
ADD "target/${app_jar}" ${HOME}/server.jar
EXPOSE 8080
ENV APM_SERVER=${APM_SERVER}
ENV MAINCLASS=example.Application
ENV MAINCLASS=org.springframework.boot.loader.JarLauncher
ENTRYPOINT java -javaagent:/home/elastic-apm-agent.jar -Delastic.apm.service_name=server -Delastic.apm.application_packages=example -Delastic.apm.server_url=http://apm-server:8200 -Djava.security.egd=file:/dev/./urandom -cp  /home/server.jar $MAINCLASS
