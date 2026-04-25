import requests


class ProcessDataTool:
    def __init__(self, config):
        """
        Centralized configuration only.
        No per-method hardcoded values.
        """

        self.base_url = config["base_url"]
        self.auth_path = config["auth_path"]
        self.process_path = config["process_path"]

        self.username = config["username"]
        self.password = config["password"]

        self.default_customer = config["default_customer"]
        self.default_what = config["default_what"]

        self.expected_message_type = config["expected_message_type"]
        self.expected_token_type = config["expected_token_type"]

        self.token = None

    def get_token(self):
        """
        Authenticate and retrieve JWT token
        """

        url = f"{self.base_url}/{self.auth_path}"

        payload = {
            "username": self.username,
            "password": self.password
        }

        response = requests.post(
            url,
            json=payload
        )

        response.raise_for_status()

        data = response.json()

        if "access_token" not in data:
            raise Exception("access_token missing from auth response")

        if data.get("token_type") != self.expected_token_type:
            raise Exception(
                f"unexpected token_type: {data.get('token_type')}"
            )

        self.token = data["access_token"]
        return self.token

    def process_data(self, customer=None, what=None):
        """
        Call protected endpoint
        """

        if not self.token:
            self.get_token()

        customer = customer or self.default_customer
        what = what or self.default_what

        url = f"{self.base_url}/{self.process_path}"

        headers = {
            "Authorization": f"Bearer {self.token}"
        }

        payload = {
            "customer": customer,
            "what": what
        }

        response = requests.post(
            url,
            headers=headers,
            json=payload
        )

        response.raise_for_status()

        return response.json()

    def validate_response(self, response, customer, what):
        """
        Validate response fields
        """

        if response.get("customerId") != customer:
            raise Exception("customerId validation failed")

        if response.get("messageType") != self.expected_message_type:
            raise Exception("messageType validation failed")

        if response.get("user") != self.username:
            raise Exception("user validation failed")

        payload = response.get("payload", {})

        if payload.get("originalWhat") != what:
            raise Exception("originalWhat validation failed")

        if payload.get("normalizedWhat") != what.upper():
            raise Exception("normalizedWhat validation failed")

        if payload.get("length") != len(what):
            raise Exception("length validation failed")

        expected_empty = (len(what) == 0)

        if payload.get("isEmpty") != expected_empty:
            raise Exception("isEmpty validation failed")

        return True

    def run(self, customer=None, what=None):
        """
        Full execution flow
        """

        customer = customer or self.default_customer
        what = what or self.default_what

        self.get_token()

        response = self.process_data(
            customer=customer,
            what=what
        )

        self.validate_response(
            response=response,
            customer=customer,
            what=what
        )

        print("SUCCESS")
        print(response)

        return response


if __name__ == "__main__":
    config = {
        "base_url": "http://localhost:8085",
        "auth_path": "auth/token",
        "process_path": "api/processdata",

        "username": "test",
        "password": "test",

        "default_customer": "test",
        "default_what": "karate validation",

        "expected_message_type": "processed",
        "expected_token_type": "Bearer"
    }

    tool = ProcessDataTool(config)
    tool.run()
