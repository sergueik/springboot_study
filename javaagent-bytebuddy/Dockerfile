FROM openjdk:8-jre-alpine3.9
ARG agent_jar="agent.jar"
ADD "agent/target/${agent_jar}" agent.jar
ARG application_jar="application.jar"
ADD "application/target/${application_jar}" application.jar
EXPOSE 8085
ENTRYPOINT ["java", "-javaagent:agent.jar", "-jar", "application.jar"]

