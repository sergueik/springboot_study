@echo OFF
REM origin: https://forums.docker.com/t/docker-on-windows-fails-with-pipe-docker-engine-the-system-cannot-find-the-file-specified/28479/4
set DOCKER_MACHINE=default
title Docker Quickstart Terminal
set DOCKER_TLS_VERIFY=1
set DOCKER_HOST=tcp://192.168.99.100:2376
set DOCKER_CERT_PATH=C:\Users\Serguei\.docker\machine\machines\%DOCKER_MACHINE%
set DOCKER_MACHINE_NAME=%DOCKER_MACHINE%
set COMPOSE_CONVERT_WINDOWS_PATHS=true
chcp 65001
"C:\Program Files\Git\bin\bash.exe" --login -i "C:\Program Files\Docker Toolbox\start.sh"
