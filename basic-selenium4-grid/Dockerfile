FROM python:3.8.12
# NOTE: Selenium 4.x requires Python version 3.7+
# while bionic has python3 version 3.6.9 and trusty has 3.5.2

ENV PYTHONBUFFERED 1
COPY ./requirements.txt /requirements.txt
RUN pip install -r /requirements.txt

RUN mkdir /app
COPY ./app /app
WORKDIR /app
