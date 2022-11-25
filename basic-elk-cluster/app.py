import socket
from flask import Flask
from elasticapm.contrib.flask import ElasticAPM
app = Flask(__name__)
app.config['ELASTIC_APM'] = {
          'SERVICE_NAME': 'FlaskApp',
          'SECRET_TOKEN': '',
          'SERVER_URL': 'http://localhost:8200'
}
apm = ElasticAPM(app)
@app.route('/')
def index():
  return "Hello World!"
if __name__ == '__main__':
  # origin: https://stackoverflow.com/questions/166506/finding-local-ip-addresses-using-pythons-stdlib
  s = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
  s.connect(('8.8.8.8', 80))
  ip = (s.getsockname()[0])
  # print([l for l in ([ip for ip in socket.gethostbyname_ex(socket.gethostname())[2] if not ip.startswith("127.")][:1], [[(s.connect(('8.8.8.8', 53)), s.getsockname()[0], s.close()) for s in [socket.socket(socket.AF_INET, socket.SOCK_DGRAM)]][0][1]]) if l][0][0])
  app.run(host = ip, port = 6000)
