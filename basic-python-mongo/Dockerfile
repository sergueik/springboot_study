FROM python:3.8.2-alpine
WORKDIR /app
RUN apk add --no-cache -t .build-deps curl ca-certificates jq gcc cmake pkgconfig make libc-dev && \
    rm -rf /var/cache/apk/*
ENV PYTHONIOENCODING=utf-8
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8 
ENV LC_LANG=C.UTF-8
RUN pip install --upgrade pip
# https://pypi.org/project/python-dotenv/
RUN pip install python-dotenv==1.0.0 fastapi pymongo
# NOTE: to install unicorn, need to add compilers
# COPY ./requirements.txt .
# RUN pip install -r requirements.txt
COPY .env app.py ./
ARG SERVICE_PORT=8080
EXPOSE $SERVICE_PORT
# ENTRYPOINT ["python", "/app/app.py"]
CMD  ["python", "/app/app.py"]

