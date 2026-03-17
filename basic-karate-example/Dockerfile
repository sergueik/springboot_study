FROM  maven:3.9.3-eclipse-temurin-11-alpine
WORKDIR /work
COPY pom.xml /work/
RUN mvn dependency:go-offline -ntp -B
ADD src /work/src
## the src is
CMD ["mvn", "test"]
