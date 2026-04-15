FROM python:3.11-slim
# TODO: mig to alpine - debian is heavy
WORKDIR /app
COPY . /app
RUN pip install -r requirements.txt --progress-bar off
ENTRYPOINT ["python", "mcp_client.py"]
CMD ["--method", "echo"]
