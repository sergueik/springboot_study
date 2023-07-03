FROM openjdk:8-jre-alpine3.9
ARG app_jar="example.properties-location.jar"
ARG LOCATION="../../root/.keys"
ENV LOCATION=${LOCATION}
WORKDIR /work/place
ADD "target/${app_jar}" app.jar
EXPOSE 8085
RUN mkdir -p ~/.keys/ && echo 123| tee /root/.keys/key.txt
ENTRYPOINT java -Dlocation=${LOCATION} -jar app.jar
