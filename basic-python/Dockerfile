FROM jfloff/alpine-python
#Copyright (c) 2020 Serguei Kouzmine
#
# beam server docker example
COPY server.py server.py
ARG SERVICE_PORT=8080
ENV SERVICE_PORT=$SERVICE_PORT 
EXPOSE $SERVICE_PORT
ENTRYPOINT ["python", "/server.py"]
