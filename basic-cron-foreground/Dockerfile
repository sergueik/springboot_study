FROM jfloff/alpine-python:3.8-slim

ARG LOGS_DIR=/var/log
# ARG PROJECT_DIR
ENV LOGS_DIR=${LOGS_DIR}
# ENV PROJECT_DIR=${PROJECT_DIR}
# ENV PYTHONDONTWRITEBYTECODE=1
# ENV PYTHONUNBUFFERED=1
# WORKDIR ${PROJECT_DIR}
# COPY . ${PROJECT_DIR}
# RUN pip install -r requirements.txt
RUN apk --no-cache update
RUN apk add --no-cache apk-cron \
	&& rm -vrf /var/cache/apk/*
COPY crontab /etc/cron.d/
RUN for file in /etc/cron.d/*; do crontab $file; done
RUN touch /var/log/cron.log && env >> /etc/environment
ENTRYPOINT ["crond", "-f"]



