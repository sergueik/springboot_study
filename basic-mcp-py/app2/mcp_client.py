import socket

from common.protocol import encode, decode

HOST = os.getenv('MCP_HOST', '127.0.0.1')
PORT = int(os.getenv('MCP_PORT', '9000'))


class MCPClient:
    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((HOST, PORT))
        self.buffer = ''
        self.id = 1

    def send(self, method, params=None):
        req = {
            'jsonrpc': '2.0',
            'id': self.id,
            'method': method
        }
        self.id += 1

        if params:
            req['params'] = params

        self.sock.sendall(encode(req).encode())
        return self.recv()

    def recv(self):
        while True:
            data = self.sock.recv(1024)
            if not data:
                raise RuntimeError('server closed')

            self.buffer += data.decode()

            if "\n" in self.buffer:
                line, self.buffer = self.buffer.split("\n", 1)
                return decode(line)

def validate_tools(response):
    if "error" in response:
        raise RuntimeError(f"Server error: {response['error']}")

    if "result" not in response:
        raise RuntimeError("Invalid response: missing result")

    tools = response["result"].get("tools")

    if tools is None:
        raise RuntimeError("Protocol error: tools is None")

    if len(tools) == 0:
        raise RuntimeError("No tools available from server")

    return tools

def main():
    c = MCPClient()

    print(c.send('initialize'))
    # dump moving along immediately 
    # should react intentionally, not silently continue.
    print(c.send('tools/list'))

    print(
        c.send(
            'tools/call',
            {
                'name': 'echo',
                'arguments': {'text': 'hello from docker network'}
            }
        )
    )


if __name__ == '__main__':
    main()
