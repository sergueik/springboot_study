FROM debian:bullseye-slim
# FROM alpine:3.9.5

ENV COLOR_RED='\033[0;31m'
ENV COLOR_GREEN='\033[0;32m'
ENV COLOR_RESET='\033[0m'
ARG ARG1=""
ENV ARG1=${ARG1}
# use bulid step to verify presence of critical args
RUN \
    if [ ! "${ARG1}" ]; then\
      ERROR_MESSAGE="ARG1 must be passed as --build-arg ARG1=...";ERROR=1; fi;\
    COLOR_RED='\033[0;31m';COLOR_RESET='\033[0m';\
    if [ ! -z "$ERROR" ]; then echo "${COLOR_RED}${ERR}${COLOR_RESET}"; \
else echo "${COLOR_GREEN}build ARG1=\"$ARG1\"${COLOR_RESET}"; fi; \
    if [ ! -z "$ERROR" ]; then  exit 1; fi

CMD ["/bin/sh", "-c", "echo ${COLOR_GREEN}build $ARG1${COLOR_RESET}"]
