FROM openjdk:8-jre-alpine3.9
ARG application_jar="application.jar"
ADD "application/target/${application_jar}" application.jar
ARG agent_jar="agent.jar"
ADD "agent/target/${agent_jar}" agent.jar
ADD index.html index.html
EXPOSE 8085
ENTRYPOINT ["java", "-javaagent:agent.jar", "-jar", "application.jar", "8085"]

