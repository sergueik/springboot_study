# NOTE: not finished
from unittest import TestCase
import pytest


@pytest.mark.usefixtures("host")
class TestRequirements(TestCase):
    """
    Check the requirements for running the bot are set up in the Docker image
    correctly
    """

    def setUp(self):
        super(TestRequirements, self).setUp()
        self.requirements = self.host.file('/src/requirements.txt')
        self.requirements_list = self.requirements.content_string
        self.pip_packages = self.host.pip_package.get_packages()

    def test_requirements_exists(self):
        """
        Check that the requirements.txt file exists
        """
        self.assertTrue(self.requirements.exists)

    def test_discord_in_reqs(self):
        """
        Check that the discord.py library exists in the requirements file
        """
        self.assertIn(
            'git+https://github.com/Rapptz/discord.py'
            '@rewrite#egg=discord.py[voice]',
            self.requirements_list
        )

    def test_pokedex_py_in_reqs(self):
        """
        Check that the pokedex.py library exists in the requirements file
        """
        self.assertIn('pokedex.py==1.1.2', self.requirements_list)

    def test_discord_installed(self):
        """
        Check that the discord.py library is installed in the python env

        Note: Check for 1.0.0 as this is what the rewrite version of discord.py
        reports itself as
        """
        self.assertIn(
            '1.0.0',
            self.pip_packages.get('discord.py').get('version')
        )

    def test_pokedex_installed(self):
        """
        Check that the pokedex.py library is installed in the python env
        """
        self.assertEqual(
            '1.1.2',
            self.pip_packages.get('pokedex.py').get('version')
        )

    def test_python_3_7(self):
        """
        Check the Python 3.7 is used when running the Python command
        """
        python_version = self.host.command('python --version').stdout
        self.assertIn('3.7', python_version)

