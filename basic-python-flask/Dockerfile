FROM python:3.8.2-alpine
ARG SERVICE_PORT=5000
WORKDIR /app
RUN apk add curl 
ENV PYTHONIOENCODING=utf-8
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8 
ENV LC_LANG=C.UTF-8

COPY ./requirements.txt swagger.json app.py ./
RUN pip install -r requirements.txt
EXPOSE $SERVICE_PORT
ENTRYPOINT ["python", "/app/app.py"]
# NOTE: adding the "-m", "flask" arguments leads to a prompt

