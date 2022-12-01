FROM python:3.8.2-alpine
COPY . /web
WORKDIR /web/api
RUN pip install -r ./requirements.txt
ENTRYPOINT ["python"]
CMD ["app.py"]
