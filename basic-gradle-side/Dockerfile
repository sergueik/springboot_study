FROM gradle:5.4.1-jdk8-alpine
WORKDIR /work
COPY build.gradle settings.gradle ./launcher/
ADD  karate  ./karate
CMD ["gradle", "-p" , "launcher", "testbuild"]
