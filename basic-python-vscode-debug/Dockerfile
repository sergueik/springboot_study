FROM python:3.8
WORKDIR /app/

COPY . /app/
RUN pip install --upgrade pip && \
    pip install -r requirements.txt

CMD uvicorn app.main:app --reload --host 0.0.0.0 --port 8000
