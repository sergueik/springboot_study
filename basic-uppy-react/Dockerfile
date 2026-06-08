FROM node:18.1.0-alpine AS react_builder

WORKDIR /app

COPY frontend /app/


RUN cd /app \
  && rm -rf node_modules package-lock.json \
  && npm install \
    @uppy/core@5.2.0 \
    @uppy/dashboard@5.1.1 \
    @uppy/xhr-upload@5.2.0 \
    @uppy/react@5.2.0 \
    @vitejs/plugin-react@4.3.4 \
    vite@5.4.19

RUN npm run build


FROM maven:3.9.5-eclipse-temurin-11-alpine as builder

WORKDIR /app

COPY pom.xml /app/
RUN mvn dependency:go-offline -q
ADD src /app/src/

COPY --from=react_builder /app/dist /app/src/main/resources/static/ 

RUN mvn clean package -DskipTests -q

FROM eclipse-temurin:11-jre-alpine as run
COPY --from=builder /app/target/example.uppy-react.jar /app/app.jar 

RUN apk update \
    && apk add --update --no-cache curl \
    && rm -rf /var/cache/* \
    && mkdir /var/cache/apk


HEALTHCHECK --interval=30s --timeout=5s --start-period=10s \
CMD curl -f http://localhost:8080/ || exit 1
ENTRYPOINT ["java", "-jar", "/app/app.jar"]
EXPOSE 8080
