FROM node:22.18.0-alpine AS react_builder

WORKDIR /app

COPY frontend /app/


RUN cd /app \
  && rm -rf node_modules package-lock.json \
  && npm install \
    @uppy/core@5.2.0 \
    @uppy/dashboard@5.1.1 \
    @uppy/xhr-upload@5.2.0 \
    @uppy/react@5.1.1

RUN npm run build


FROM maven:3.9.5-eclipse-temurin-11-alpine as builder

WORKDIR /app

COPY backend /app/

# copy React static frontend bundle 
COPY --from=react_builder /app/dist /app/src/main/resources/static/ 

RUN cd /app && mvn dependency:go-offline -q
RUN cd /app && mvn clean package -DskipTests

FROM eclipse-temurin:11-jre-alpine as run
COPY --from=builder /app/target/example.uppy-react-multipart-upload-backend.jar /app/app.jar 

ENTRYPOINT ["java", "-jar", "/app/app.jar"]
EXPOSE 8080
