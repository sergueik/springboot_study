FROM python:3.8.2-alpine
WORKDIR /web/api
ADD . /web/
RUN pip install -r requirements.txt
ENTRYPOINT ["python"]
CMD ["app.py"]
