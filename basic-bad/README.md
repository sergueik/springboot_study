### Usage

on Linux host,
```sh
docker-compose up --build
```
works

however under __Docker Toolbox__, the `docker-compose` (both versions __1.2.x__ and __2.20.x__) is failing with:
```text
Creating network "basic-bad_test" with driver "bridge"
Building test
Step 1/5 : FROM alpine:3.9.5
---> 82f67be598eb
Step 2/5 : WORKDIR /code
---> Using cache
---> 8570a2595ca3
Step 3/5 : COPY entrypoint.sh ./
---> Using cache
---> 9fa97a082554
Step 4/5 : RUN sed -i  's|\r||g' ./entrypoint.sh && chmod +x ./entrypoint.sh
---> Using cache
---> 973ee500749a
Step 5/5 : ENTRYPOINT [ "./entrypoint.sh" ]
---> Using cache
---> cdb8041f7b18
Successfully built cdb8041f7b18
Successfully tagged test:1.0.0
Creating test ... error

ERROR: for test  Cannot start service test: OCI runtime create failed: container_linux.go:349: starting container process caused "exec: \"./entrypoint.sh\": stat ./entrypoint.sh: no such file or directory": unknown

ERROR: for test  Cannot start service test: OCI runtime create failed: container_linux.go:349: starting container process caused "exec: \"./entrypoint.sh\": stat ./entrypoint.sh: no such file or directory": unknown
ERROR: Encountered errors while bringing up the project.
```
replacing 
```sh
ENTRYPOINT [ "./entrypoint.sh" ]
```
with
```sh
ENTRYPOINT /code/entrypoint.sh
```

leads to a slightly different format of the error message but `docker-compose` still fails:
```text
Building test
Step 1/5 : FROM alpine:3.9.5
 ---> 82f67be598eb
Step 2/5 : WORKDIR /code
 ---> Using cache
 ---> 8570a2595ca3
Step 3/5 : COPY entrypoint.sh ./
 ---> 35d261faff9e
Step 4/5 : RUN sed -i  's|\r||g' ./entrypoint.sh && chmod +x ./entrypoint.sh
 ---> Running in 7a2ab3fed074
Removing intermediate container 7a2ab3fed074
 ---> 3a163f0624ed
Step 5/5 : ENTRYPOINT /code/entrypoint.sh
 ---> Running in e4baf9bf3512
Removing intermediate container e4baf9bf3512
 ---> da4019e000e8
Successfully built da4019e000e8
Successfully tagged test:1.0.0
Recreating test ... done
Attaching to test
test    | /bin/sh: /code/entrypoint.sh: not found
```
the `dockrer build`, `docker run` work fine


### Author
[Serguei Kouzmine](kouzmine_serguei@yahoo.com)
