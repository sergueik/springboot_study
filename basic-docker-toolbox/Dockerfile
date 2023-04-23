FROM python:3.8.2-alpine
WORKDIR /app
RUN apk add curl 
ENV LANG=C.UTF-8
RUN pip install --upgrade pip
COPY ./requirements.txt ./
RUN pip install -r requirements.txt
COPY app.py ./
EXPOSE 5000
ENTRYPOINT ["python", "/app/app.py"]

