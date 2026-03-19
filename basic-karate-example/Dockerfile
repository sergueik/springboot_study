FROM  maven:3.9.3-eclipse-temurin-11-alpine
WORKDIR /work
COPY pom.xml .
RUN mvn dependency:go-offline -ntp -B
COPY dummy/src src
## bypass maven’s lazy plugin resolution without actually running the tests
RUN mvn -B -ntp -DskipTests surefire:test
COPY src src
CMD ["mvn", "test"]
