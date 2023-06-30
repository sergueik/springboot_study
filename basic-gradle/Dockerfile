ARG BASE=gradle:7.3.1-jdk11-alpine
FROM $BASE 
WORKDIR /work
ADD src ./src/

COPY build.gradle dependencies.gradle versioning.gradle settings.gradle ./
CMD ["gradle", "build"]

