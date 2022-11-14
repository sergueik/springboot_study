FROM openjdk:8-jre-alpine3.9
# FROM openjdk:8-jdk-alpine
VOLUME /tmp
ARG HOME='/home'
ARG JAR_FILE="target/example.static_page.jar"
ARG ELASTIC_APM_AGENT_VERSION="1.20.0" 
RUN wget -O ${HOME}/elastic-apm-agent.jar https://search.maven.org/remotecontent?filepath=co/elastic/apm/elastic-apm-agent/${ELASTIC_APM_AGENT_VERSION}/elastic-apm-agent-${ELASTIC_APM_AGENT_VERSION}.jar
# COPY elastic-apm-agent.jar ${HOME}/
COPY ${JAR_FILE} ${HOME}/app.static.jar
# found obsolete app.jar, unclear how to prune. this was a attempted workaround
# WORKDIR $HOME
# cannot normalize nothing
WORKDIR /home

# Error occurred during initialization of VM
# agent library failed to init: instrument
# Error opening zip file or JAR manifest missing : ${HOME}/elastic-apm-agent.jar

# ENTRYPOINT [ "java", "-javaagent:${HOME}/elastic-apm-agent.jar", "-Djava.security.egd=file:/dev/./urandom", "-jar", "${HOME}/app.jar" ]
# TODO: configure through Runtime attach parameters
ENTRYPOINT [ "java", "-javaagent:/home/elastic-apm-agent.jar", "-Djava.security.egd=file:/dev/./urandom", "-jar", "/home/app.static.jar" ]
