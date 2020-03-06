FROM jfloff/alpine-python
#Copyright (c) 2020 Serguei Kouzmine
#
# beam server docker prototype
ADD "/server.py" "server.py"
ARG PORT=8080
ARG category
ENV PORT=$PORT 
EXPOSE $PORT
ENTRYPOINT ["python", "/server.py"]
