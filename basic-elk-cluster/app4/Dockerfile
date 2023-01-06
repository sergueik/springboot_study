FROM python:3.8.2-alpine
WORKDIR /app
RUN apk add curl lynx
ENV PYTHONIOENCODING=utf-8
ENV PYTHONDONTWRITEBYTECODE 1
ENV PYTHONUNBUFFERED 1
ENV LANG=C.UTF-8
ENV LC_ALL=C.UTF-8 
ENV LC_LANG=C.UTF-8
RUN pip install --upgrade pip
COPY ./requirements.txt .
RUN pip install -r requirements.txt
COPY app.py .
ADD Templates Templates
ARG SERVICE_PORT=5000
EXPOSE $SERVICE_PORT
ENTRYPOINT ["python", "/app/app.py"]
