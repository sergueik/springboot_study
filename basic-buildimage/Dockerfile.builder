FROM azul/zulu-openjdk-alpine:11
WORKDIR application

ENV MAVEN_VERSION 3.8.1
ENV MAVEN_HOME /usr/lib/mvn
ENV PATH $MAVEN_HOME/bin:$PATH

RUN wget http://archive.apache.org/dist/maven/maven-3/$MAVEN_VERSION/binaries/apache-maven-$MAVEN_VERSION-bin.tar.gz && \
  tar -zxvf apache-maven-$MAVEN_VERSION-bin.tar.gz && \
  rm apache-maven-$MAVEN_VERSION-bin.tar.gz && \
  mv apache-maven-$MAVEN_VERSION /usr/lib/mvn

COPY pom.xml.jdk11 /application
RUN mv pom.xml.jdk11 pom.xml
COPY src /application/src
ARG JAR_FILE=/application/target/example.buildimage.jar 

RUN mvn clean package
RUN java -Djarmode=layertools -jar $JAR_FILE extract
