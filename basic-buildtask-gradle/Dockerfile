ARG BASE=gradle:5.4.1-jdk8-alpine
FROM $BASE 
WORKDIR /work
ADD buildSrc ./buildSrc/
COPY build.gradle dependencies.gradle versioning.gradle settings.gradle ./
CMD ["gradle", "clean", "build", "printGroovyVersionBuildSrc", "printJavaVersionBuildSrc"]
