ARG BASE=gradle:5.4.1-jdk8-alpine
FROM $BASE 
WORKDIR /work
ADD buildSrc ./buildSrc/

COPY build.gradle dependencies.gradle versioning.gradle settings.gradle ./
# RUN gradle build
CMD ["gradle", "clean", "build", "updateUdeployProperties", "updateApplicationProperties"]
