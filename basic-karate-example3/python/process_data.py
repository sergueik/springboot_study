import argparse
import json
import requests
import sys

class ProcessDataTool:
    def __init__(self, config):
        self.config = config
        self.token = None

    def get_token(self):
        url = f"{self.config['base_url']}/{self.config['auth_path']}"

        payload = {
            "username": self.config["username"],
            "password": self.config["password"]
        }

        response = requests.post(url, json=payload)
        response.raise_for_status()

        data = response.json()

        if "access_token" not in data:
            raise Exception("access_token missing from auth response")

        if data.get("token_type") != self.config["expected_token_type"]:
            raise Exception("unexpected token_type")

        self.token = data["access_token"]
        return self.token

    def process_data(self):
        if not self.token:
            self.get_token()

        url = f"{self.config['base_url']}/{self.config['process_path']}"

        headers = {
            "Authorization": f"Bearer {self.token}"
        }

        payload = {
            "customer": self.config["default_customer"],
            "what": self.config["default_what"]
        }

        response = requests.post(
            url,
            headers=headers,
            json=payload
        )

        response.raise_for_status()
        return response.json()

    def validate_response(self, response):
        customer = self.config["default_customer"]
        what = self.config["default_what"]

        if response.get("customerId") != customer:
            raise Exception("customerId validation failed")

        if response.get("messageType") != self.config["expected_message_type"]:
            raise Exception("messageType validation failed")

        if response.get("user") != self.config["username"]:
            raise Exception("user validation failed")

        payload = response.get("payload", {})

        if payload.get("originalWhat") != what:
            raise Exception("originalWhat validation failed")

        if payload.get("normalizedWhat") != what.upper():
            raise Exception("normalizedWhat validation failed")

        if payload.get("length") != len(what):
            raise Exception("length validation failed")

        if payload.get("isEmpty") != (len(what) == 0):
            raise Exception("isEmpty validation failed")

        return True

    def run(self):
        self.get_token()
        response = self.process_data()
        self.validate_response(response)

        print("SUCCESS", file=sys.stderr)
        print(response)

        return response


def load_default_config():
    return json.loads(DEFAULT_CONFIG_JSON)


def load_file_config(config_file):
    if not config_file:
        return {}

    with open(config_file, "r", encoding="utf-8") as f:
        return json.load(f)


def parse_arguments():
    parser = argparse.ArgumentParser(
        description="Process data tool"
    )

    parser.add_argument(
        "--config-file",
        help="Optional external JSON config file"
    )

    parser.add_argument("--base-url")
    parser.add_argument("--username")
    parser.add_argument("--password")
    parser.add_argument("--customer")
    parser.add_argument("--what")

    args = parser.parse_args()

    return {
        "config_file": args.config_file,
        "base_url": args.base_url,
        "username": args.username,
        "password": args.password,
        "default_customer": args.customer,
        "default_what": args.what
    }


def merge_config(default_config, file_config, cli_args):
    merged = dict(default_config)

    for source in [file_config, cli_args]:
        for key, value in source.items():
            if value is not None:
                merged[key] = value

    return merged


DEFAULT_CONFIG_JSON = """
{
  "base_url": "http://localhost:8085",
  "auth_path": "auth/token",
  "process_path": "api/processdata",
  "username": "test",
  "password": "test",
  "default_customer": "test",
  "default_what": "python validation",
  "expected_message_type": "processed",
  "expected_token_type": "Bearer"
}
"""



if __name__ == "__main__":
    default_config = load_default_config()
    cli_args = parse_arguments()
    file_config = load_file_config(cli_args.get("config_file"))

    final_config = merge_config(
        default_config,
        file_config,
        cli_args
    )

    tool = ProcessDataTool(final_config)
    tool.run()

