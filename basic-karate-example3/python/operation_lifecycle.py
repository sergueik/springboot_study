import sys
import re
import requests
import json
from typing import Dict, Any
import yaml


class OperationLifecycle:
    """
    Pure YAML-driven execution engine.
    No CONFIG, no hardcoded workflow.
    """

    def __init__(self, yaml_path: str):
        with open(yaml_path, "r", encoding="utf-8") as f:
            self.spec = yaml.safe_load(f)

        self.base_url = self.spec["base_url"]
        self.credentials = self.spec.get("credentials", {})
        self.workflow = self.spec.get("workflow", [])

        self.context: Dict[str, Any] = {}
        self.token = None

    # -------------------------
    # AUTH
    # -------------------------
    def _get_token(self):
        resp = requests.post(
            f"{self.base_url}/auth/token",
            json=self.credentials
        )
        resp.raise_for_status()
        self.token = resp.json()["access_token"]
        self.context["token"] = self.token

    # -------------------------
    # TEMPLATE RESOLVER
    # -------------------------
    def _resolve(self, value):
        if isinstance(value, str):
            for k, v in self.context.items():
                value = value.replace(f"${{{k}}}", str(v))
        return value

    def _resolve_obj(self, obj):
        if isinstance(obj, dict):
            return {k: self._resolve_obj(v) for k, v in obj.items()}
        if isinstance(obj, list):
            return [self._resolve_obj(x) for x in obj]
        return self._resolve(obj)

    # -------------------------
    # EXECUTION
    # -------------------------
    def run(self):

        self._get_token()

        results = {}

        for step in self.workflow:

            action = step["action"]
            endpoint = self._resolve(step.get("endpoint", ""))
            print(f'endpoint: {endpoint}', file=sys.stderr)
            body = self._resolve_obj(step.get("body", {}))
            headers = step.get("headers", {})

            # inject auth automatically if needed
            if "Authorization" in headers:
                headers = self._resolve_obj(headers)
            else:
                headers = {
                    "Authorization": f"Bearer {self.token}",
                    "Content-Type": "application/json"
                }

            url = f"{self.base_url}{endpoint}"

            if action == "post":
                resp = requests.post(url, json=body, headers=headers)
                resp.raise_for_status()
                data = resp.json()
                print(f'data: {data}', file=sys.stderr)
                self.context["created"] = data
                print(f'context: {self.context}', file=sys.stderr)
                endpoint = self._resolve(step.get("endpoint", ""))
                print(f'endpoint: {endpoint}', file=sys.stderr)
                results[step["name"]] = data

            elif action == "put":

                print(f'put url: {url}', file=sys.stderr)
                resp = requests.put(url, json=body, headers=headers)
                resp.raise_for_status()
                results[step["name"]] = "ok"

            elif action == "get":
                resp = requests.get(url, headers=headers)
                results[step["name"]] = resp.json()

            elif action == "delete":
                resp = requests.delete(url, headers=headers)
                results[step["name"]] = resp.status_code

        return results


if __name__ == "__main__":

    tool = OperationLifecycle('operation_lifecycle.yaml')
    result = tool.run()

    print(json.dumps(result, indent=2))
