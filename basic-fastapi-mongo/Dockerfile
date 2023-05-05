FROM python:3.8.2-alpine
WORKDIR /app
RUN apk add --no-cache -t .build-deps curl ca-certificates jq gcc cmake pkgconfig make libc-dev linux-headers && \
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
# RUN pip install unicorn
RUN pip install uvicorn
COPY .env app.py ./
ARG SERVICE_PORT=8080
EXPOSE $SERVICE_PORT
# ENTRYPOINT ["python", "/app/app.py"]
# CMD  ["python", "/app/app.py"]
CMD ["uvicorn" ,"app:app" ,"--host" ,"0.0.0.0" ,"--reload"]
