# Copyright (c) 2020 Serguei Kouzmine

# supervisor docker example
FROM jfloff/alpine-python
RUN apk add --no-cache supervisor && rm -rf /var/lib/apt/lists/*
RUN sed -i 's/^\(\[supervisord\]\)$/\1\nnodaemon=true/' /etc/supervisord.conf
RUN mkdir -p /etc/supervisor.d/
COPY server.py server.py
COPY agent.conf etc/supervisor.d/agent.ini
# for server.py
ARG SERVICE_PORT=8080
ENV SERVICE_PORT=$SERVICE_PORT
EXPOSE $SERVICE_PORT
# NOTE: not ENTRYPOINT
CMD ["supervisord", "-c", "/etc/supervisord.conf"]
