FROM basic-python-crypto
WORKDIR /app
ARG PBKDF_VERSION=1.3
COPY app1.py app3.py ./
COPY ./pbkdf2-$PBKDF_VERSION/pbkdf2.py  ./

ARG SERVICE_PORT=8080
EXPOSE $SERVICE_PORT
# ENTRYPOINT ["python", "/app/app.py"]
