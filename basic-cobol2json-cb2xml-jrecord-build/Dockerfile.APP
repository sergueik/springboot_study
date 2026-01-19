FROM eclipse-temurin:11-jre-alpine

WORKDIR /app

COPY --from=cobol2json /build/src/target/cobolToJson-0.93.3.jar /app/cobolToJson.jar

# NOTR: no longer have to collect the 
# dependency jars from cobol2json's local Maven repo
# COPY --from=cobol2json /root/.m2/repository/ ./lib/

# NOTE: no longer need to construct the complex classpath to include 
# the main application jar and all dependencies
# ENV CLASSPATH=/app/cobolToJson.jar:/app/lib/*


ENTRYPOINT ["java", "-jar", "/app/cobolToJson.jar"]

