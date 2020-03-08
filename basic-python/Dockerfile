# Copyright (c) 2020 Serguei Kouzmine
#
# beam server docker example
FROM jfloff/alpine-python
COPY server.py server.py
COPY background_server.py background_server.py
ARG SERVICE_PORT=8080
ENV SERVICE_PORT=$SERVICE_PORT
EXPOSE $SERVICE_PORT
ENTRYPOINT ["python", "/background_server.py"]
# ENTRYPOINT ["python", "/server.py"]
