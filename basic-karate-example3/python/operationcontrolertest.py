import requests
import json
from typing import Dict, Any


class OperationLifecycleTool:
    """
    Single MCP-style tool that executes full CRUD lifecycle.
    """

    def __init__(self, config: Dict[str, Any]):
        self.base_url = config.get("base_url", "http://localhost:8085")
        self.credentials = config.get("credentials", {})

        self.token = None
        self.last_id = None

    # -------------------------
    # INTERNAL AUTH
    # -------------------------
    def _get_token(self):
        resp = requests.post(
            f"{self.base_url}/auth/token",
            json=self.credentials
        )
        resp.raise_for_status()
        self.token = resp.json()["access_token"]

    def _headers(self):
        return {
            "Authorization": f"Bearer {self.token}",
            "Content-Type": "application/json"
        }

    # -------------------------
    # FULL WORKFLOW TOOL (ONLY PUBLIC METHOD)
    # -------------------------
    def run(self, what: str = "hello") -> Dict[str, Any]:

        self._get_token()

        # CREATE
        create_resp = requests.post(
            f"{self.base_url}/operation",
            headers=self._headers(),
            json={"what": what}
        )
        create_resp.raise_for_status()
        op_id = create_resp.json()["id"]

        # UPDATE
        requests.put(
            f"{self.base_url}/operation/{op_id}",
            headers=self._headers(),
            json={"what": "updated"}
        ).raise_for_status()

        # GET
        get_resp = requests.get(
            f"{self.base_url}/operation/{op_id}",
            headers=self._headers()
        )
        get_data = get_resp.json()

        # DELETE
        delete_status = requests.delete(
            f"{self.base_url}/operation/{op_id}",
            headers=self._headers()
        ).status_code

        # VERIFY DELETION
        final_get = requests.get(
            f"{self.base_url}/operation/{op_id}",
            headers=self._headers()
        )

        return {
            "id": op_id,
            "created": True,
            "updated": True,
            "get_after_update": get_data,
            "delete_status": delete_status,
            "exists_after_delete": final_get.status_code != 404
        }


# -------------------------
# CONFIG (single entry point style)
# -------------------------
CONFIG = {
    "base_url": "http://localhost:8085",
    "credentials": {
        "username": "test",
        "password": "test"
    }
}


# -------------------------
# ENTRY POINT (MCP TOOL STYLE)
# -------------------------
if __name__ == "__main__":
    tool = OperationLifecycleTool(CONFIG)

    result = tool.run(what="karate validation")

    print(json.dumps(result, indent=2))
