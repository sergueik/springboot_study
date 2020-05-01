FROM openjdk:8-jdk-alpine3.9
ADD EnvironmentPrinter.java .
RUN javac EnvironmentPrinter.java 
ENTRYPOINT ["java", "EnvironmentPrinter"] 
