# origin: https://github.com/philpep/testinfra/blob/master/doc/source/examples.rst
import pytest
import subprocess
import testinfra
import re

image = 'myimage'

@pytest.fixture(scope='session')
def host(request):
  subprocess.check_call(['docker', 'build', '-t', image, '.'])
  _id = subprocess.check_output( ['docker', 'run', '-d', image]).decode().strip()
  yield testinfra.get_host("docker://" + _id)
  subprocess.check_call(['docker', 'rm', '-f', _id])


def test_myimage(host):
  assert host.check_output('java -version') == ''
  assert re.search('openjdk version "1.8.0_212"', host.check_output('java -version 2>&1')) != None
  assert re.search('/usr/bin/java is owned by java-common.*', host.check_output('apk info --who-owns /usr/bin/java')) != None

