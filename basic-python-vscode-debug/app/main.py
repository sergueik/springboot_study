from fastapi import FastAPI, Request
from datetime import datetime
import httpx
import os

app = FastAPI()

TARGET_HOST = os.getenv("TARGET_HOST", "app")
TARGET_PORT = os.getenv("TARGET_PORT", "80")

@app.get("/")
async def home():
    hello_world = "Hello World!"
    return hello_world

@app.get("/data")
async def proxy_data(request: Request, ts: str = None):
    # Use ts from query param if given, fallback to current unix timestamp
    ts_value = ts or str(int(datetime.utcnow().timestamp()))
    url = f"http://{TARGET_HOST}:{TARGET_PORT}/data?ts={ts_value}"
    async with httpx.AsyncClient() as client:
        resp = await client.get(url)
        return resp.json()


