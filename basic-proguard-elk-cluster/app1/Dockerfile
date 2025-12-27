FROM eclipse-temurin:11-jre-alpine
ARG HOME='/home'
RUN apk add curl netcat-openbsd \
  && rm -vrf /var/cache/apk/*
WORKDIR /home
ARG ELASTIC_APM_AGENT_VERSION="1.35.0" 
ARG APM_SERVER="apm_server"
RUN wget -O ${HOME}/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar
ARG app_jar="example.relay.jar"
ADD "target/${app_jar}" ${HOME}/relay.jar
EXPOSE 8080
ENV APM_SERVER=${APM_SERVER}
# ENV SERVER will be added from docker-compose.yml
# With springboot, cannot use a standard Java -cp command to launch the application's main class directly. 
# the Main-Class defined in the JAR's manifest file points to 
# Spring internal JarLauncher
# org.springframework.boot.loader.JarLauncher
ENV MAINCLASS=example.Application
ENV MAINCLASS=org.springframework.boot.loader.JarLauncher
# NOTE: env is ignored in exec form JSON array syntax
ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Delastic.apm.service_name=relay", "-Delastic.apm.application_packages=example", "-Delastic.apm.server_url=http://apm-server:8200", "-Djava.security.egd=file:/dev/./urandom", "-cp", "/home/relay.jar", "org.springframework.boot.loader.JarLauncher"]
