import socket, os, socket, json, time, sys
import logging
import argparse

# import jsonschema # currently unused
# comment unless run in container

from common.protocol import encode, decode
logging.basicConfig(
    level=logging.INFO,
    stream=sys.stdout,
    format='%(asctime)s | %(levelname)s | %(message)s'
)

log = logging.getLogger('mcp-client')


HOST = os.getenv('MCP_HOST', '127.0.0.1')
PORT = int(os.getenv('MCP_PORT', '9000'))


class MCPClient:
    def __init__(self):
        self.sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
        self.sock.connect((HOST, PORT))
        self.buffer = ''
        self.id = 1

    def send(self, method, params=None):
        req_id = self.id
        self.id += 1

        msg = {
            'jsonrpc': '2.0',
            'id': req_id,
            'method': method
        }

        if params:
            msg['params'] = params

        raw = json.dumps(msg) + "\n"

        log.info(f"SEND [{method}] id={req_id} payload={msg}")

        start = time.time()
        self.sock.sendall(raw.encode())

        resp = self.recv()

        duration = (time.time() - start) * 1000

        log.info(f"RECV id={req_id} time={duration:.2f}ms response={resp}")

        return resp


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
    if 'error' in response:
        raise RuntimeError(f"Server error: {response['error']}")

    if 'result' not in response:
        raise RuntimeError('Invalid response: missing result')

    tools = response['result'].get('tools')

    if tools is None:
        raise RuntimeError('Protocol error: tools is None')

    if len(tools) == 0:
        raise RuntimeError('No tools available from server')

    return tools

def main():
    parser = argparse.ArgumentParser(prog = 'mcp_client.py')
    parser.add_argument('--method', '-m', help = 'method to call', type = str, action = 'store')
    parser.add_argument('--debug', '-d', help = 'debug', action = 'store_const', const = True)

    args = parser.parse_args()
    if args.debug:
      print('running debug mode')
      print('method'.format(args.method))

    if args.method == None :
      parser.print_help()
      exit(1)
    method = args.method
    mcp_client = MCPClient()

    print(mcp_client.send('initialize'))

    resp = mcp_client.send('tools/list')

    try:
        tools = validate_tools(resp)
    except Exception as e:
        log.error(f"Tool discovery failed: {e}")
        return
    if method not in {tool['name'] for tool in tools }:
        log.error('Required tool method "{}" is NOT available — exiting'.format(method))
        log.info(f"Available tools: {[tool['name'] for tool in tools]}")
        sys.exit(1)

    resp = mcp_client.send(
        'tools/call',
        {
            'name': method,
            'arguments': {'what': 'hello from mcp client'}
        }
    )
    # NOTE: to avoid brittle indexing:
    # content = resp.get('result', {}).get('content')
    log.info(f"{method} response content: {resp['result']['content']}")

if __name__ == '__main__':
    main()
