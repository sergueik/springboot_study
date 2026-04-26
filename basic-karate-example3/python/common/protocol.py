import json

def encode(msg: dict) -> str:
    return json.dumps(msg) + "\n"

def decode(line: str) -> dict:
    return json.loads(line)
