
# 1st stage: Extract the layers
FROM maven:3.9.3-eclipse-temurin-11-alpine as builder
WORKDIR app
COPY pom.xml .
RUN mvn dependency:go-offline -B
ARG JAR_FILE=target/example.layered.jar
ENV JAR_FILE $JAR_FILE
# 2nd stage: package and extract the layers
# Optional have a extra temporary container for extraction
COPY src ./src
RUN mvn clean package -DskipTests -B

RUN apk add --no-cache unzip

# show layers index
RUN echo "==== BOOT-INF/layers.idx ====" \
 && unzip -p $JAR_FILE BOOT-INF/layers.idx || echo 'BOOT-INF/layers.idx not found'

RUN java -Djarmode=layertools -jar $JAR_FILE extract || echo "something went wrong but we are ignoring it"
RUN find . -maxdepth 2 -path ./target -prune -o \( -type d -a -print \)
ENTRYPOINT ""
