ARG BASE=gradle:5.4.1-jdk8-alpine
FROM $BASE 
WORKDIR /work
ADD buildSrc ./buildSrc/

COPY build.gradle dependencies.gradle versioning.gradle settings.gradle ./
RUN gradle :buildSrc:compileJava :buildSrc:compileGradle || echo 'Ignore the build error - need the build cache'
CMD ["gradle", "updateUdeployProperties", "updateApplicationProperties"]
