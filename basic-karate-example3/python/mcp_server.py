import sys
import os
import socket
from common.protocol import encode, decode

from operation_lifecycle import OperationLifecycle

def run_operation_lifecycle(args):
    what = args.get('what', 'hello')

    operation_lifecycle = OperationLifecycle(CONFIG)

    return operation_lifecycle.run(what=what)

def handle_request(req: dict):
    method = req.get('method')
    req_id = req.get('id')
    # plain REST health check very standard in MCP-style services and microservices in general
    if method == 'health':
        return {'status': 'ok'}

    if method == 'initialize':
        return {
            'jsonrpc': '2.0',
            'id': req_id,
            'result': {'status': 'ok', 'server': 'mcp-lab'}
        }

    if method == 'tools/list':
        return {
            'jsonrpc': '2.0',
            'id': req_id,
            'result': {
                'tools': [
                    {'name': key, 'description': val['description']}
                    for key, val in TOOLS.items()
                ]
            }
        }

    if method == 'tools/call':
        params = req.get('params', {})
        name = params.get('name')
        args = params.get('arguments', {})

        if name not in TOOLS:
            return {
                'jsonrpc': '2.0',
                'id': req_id,
                'error': {'message': f"Unknown tool: {name}"}
            }

        print(f'Invokig tool "name" with ({args})', file=sys.stderr)
        result = TOOLS[name]['handler'](args)

        return {
            'jsonrpc': '2.0',
            'id': req_id,
            'result': {'content': result}
        }

    return {
        'jsonrpc': '2.0',
        'id': req_id,
        'error': {'message': f"Unknown method: {method}"}
    }


# NOTE: cannot instantiate before the def run_operation_lifecycle
TOOLS = {
    'echo': {
        'description': 'Echo input text',
        'handler': lambda args: args.get('text', '')
    },
    'uppercase': {
        'description': 'Uppercase text',
        'handler': lambda args: args.get('text', '').upper()
    },
    'operation_lifecycle': {
        'description': 'Execute full CRUD lifecycle',
        'handler': run_operation_lifecycle
    }    
}

CONFIG = {
    'base_url': 'http://localhost:8085',
    'credentials': {
        'username': 'test',
        'password': 'test'
    }
}

HOST = os.getenv('MCP_BIND', '0.0.0.0')
PORT = int(os.getenv('MCP_PORT', '9000'))

def main():
    s = socket.socket(socket.AF_INET, socket.SOCK_STREAM)

    s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)

    s.bind((HOST, PORT))
    s.listen(10)

    print(f"[server] listening on {HOST}:{PORT}")

    while True:
        conn, addr = s.accept()
        print(f"[server] client: {addr}")

        buffer = ''

        with conn:
            while True:
                data = conn.recv(1024)
                if not data:
                    break

                buffer += data.decode()

                while "\n" in buffer:
                    line, buffer = buffer.split("\n", 1)
                    if not line.strip():
                        continue

                    req = decode(line)
                    resp = handle_request(req)
                    conn.sendall(encode(resp).encode())


if __name__ == '__main__':
    main()
