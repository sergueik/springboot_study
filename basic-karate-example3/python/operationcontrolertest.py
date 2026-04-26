"""
OperationController lifecycle tool (Karate → Python conversion)
"""

import requests
import json
from typing import Dict, Any


class OperationControllerTool:
    """
    Stateless workflow runner that mimics Karate feature execution.
    """

    def __init__(self, config: Dict[str, Any]):
        self.base_url = config.get("base_url", "http://localhost:8085")
        self.credentials = config.get("credentials", {"username": "test", "password": "test"})

        self.token = None
        self.last_id = None

    # -------------------------
    # AUTH
    # -------------------------
    def get_token(self) -> str:
        url = f"{self.base_url}/auth/token"

        resp = requests.post(url, json=self.credentials)
        resp.raise_for_status()

        data = resp.json()
        self.token = data["access_token"]

        return self.token

    # -------------------------
    # HEADERS
    # -------------------------
    def _headers(self) -> Dict[str, str]:
        return {
            "Authorization": f"Bearer {self.token}",
            "Content-Type": "application/json"
        }

    # -------------------------
    # CREATE
    # -------------------------
    def create_operation(self, what: str) -> str:
        url = f"{self.base_url}/operation"

        resp = requests.post(
            url,
            headers=self._headers(),
            json={"what": what}
        )

        resp.raise_for_status()
        data = resp.json()

        self.last_id = data["id"]
        return self.last_id

    # -------------------------
    # UPDATE
    # -------------------------
    def update_operation(self, op_id: str, what: str) -> None:
        url = f"{self.base_url}/operation/{op_id}"

        resp = requests.put(
            url,
            headers=self._headers(),
            json={"what": what}
        )

        resp.raise_for_status()

    # -------------------------
    # GET
    # -------------------------
    def get_operation(self, op_id: str) -> Dict[str, Any]:
        url = f"{self.base_url}/operation/{op_id}"

        resp = requests.get(url, headers=self._headers())

        if resp.status_code == 404:
            return {"exists": False}

        resp.raise_for_status()
        return resp.json()

    # -------------------------
    # DELETE
    # -------------------------
    def delete_operation(self, op_id: str) -> int:
        url = f"{self.base_url}/operation/{op_id}"

        resp = requests.delete(url, headers=self._headers())
        return resp.status_code

    # -------------------------
    # FULL WORKFLOW
    # -------------------------
    def run_full_lifecycle(self) -> Dict[str, Any]:
        self.get_token()

        op_id = self.create_operation("hello")
        self.update_operation(op_id, "updated")

        updated = self.get_operation(op_id)

        delete_status = self.delete_operation(op_id)
        after_delete = self.get_operation(op_id)

        return {
            "id": op_id,
            "updated": updated,
            "delete_status": delete_status,
            "after_delete": after_delete
        }


# -------------------------
# CONFIG (your preferred style)
# -------------------------
CONFIG = {
    "base_url": "http://localhost:8085",
    "credentials": {
        "username": "test",
        "password": "test"
    }
}


# -------------------------
# ENTRY POINT
# -------------------------
if __name__ == "__main__":
    tool = OperationControllerTool(CONFIG)

    result = tool.run_full_lifecycle()

    print(json.dumps(result, indent=2))
