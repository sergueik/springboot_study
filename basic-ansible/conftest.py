import pytest
import subprocess
import os
import testinfra
# NOTE: not finished
DOCKER_IMAGE_NAME = 'example-play'
# scope='session' uses the same container for all the tests;
# scope='function' uses a new container per test function.
@pytest.fixture(scope='class')
def host(request):
    # run a container
    docker_id = subprocess.check_output(
        [
            'docker',
            'run',
            '-d',
            DOCKER_IMAGE_NAME
        ]
    ).decode().strip()
    # return a testinfra connection to the container
    host = testinfra.get_host("docker://" + docker_id)
    request.cls.host = host
    yield host
    # at the end of the test suite, destroy the container
    subprocess.check_call(['docker', 'rm', '-f', docker_id])
