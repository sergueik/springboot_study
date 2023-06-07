import getopt
import sys
import os
import re
import subprocess
from pprint import pprint


class ServiceConfig(object):
    # NOTE: the 'object' super class should be named in all lower case:

    def __init__(self, filename=None, path=None, debug=False):
        self._file = filename
        self._path = path
        self._debug = debug
        self._lines = []
        self._config = []

    @property
    def filename(self):
        return self._filename

    @property
    def path(self):
        return self._path

    @property
    def debug(self):
        return self._debug

    @property
    def lines(self):
        self._lines = []
        self.__read_file__()
        self.__process_execs__()
        self.__process_includes__()
        return self._lines

    @property
    def config(self):
        self._config = []
        self.__read_config__()
        return self._config

    @filename.setter
    def filename(self, value):
        self._filename = value

    @path.setter
    def path(self, value):
        self._path = value

    @debug.setter
    def debug(self, value):
        self._debug = value

    # methods

    def __read_file__(self):
        with open(os.path.join(self.path, self.filename), mode='r') as file:
            for line in file:
                self._lines.append(line.strip())

    def __process_includes__(self):
        patt = re.compile('^#include ')
        scanned = False
        recursion_control_cnt = 0
        while not scanned:
            scanned = True
            for cnt in range(len(self._lines)):

                line = self._lines[cnt]
                if line.find(' ') == -1:
                    continue

                if not patt.match(line) is None:
                    _, filename, *_ = line.split(' ')
                    if os.path.isfile(os.path.join(self.path, filename)):
                        service_config = ServiceConfig()
                        service_config.path = self.path
                        service_config.filename = filename
                        insert_lines = service_config.lines
                        scanned = False
                        recursion_control_cnt = recursion_control_cnt + 1
                        # abort recursion which went too far
                        if recursion_control_cnt > 5:
                            scanned = True
                        self._lines = self._lines[0:cnt] + insert_lines + self._lines[cnt + 1:]

    def __process_execs__(self):
        patt = re.compile('^#exec ')
        for cnt in range(len(self._lines)):
            line = self._lines[cnt]
            if line.find(' ') == -1:
                continue
            if not patt.match(line) is None:
                _, script, *_ = line.split(' ')
                script_path = os.path.join(self.path, script)
                if os.path.isfile( script_path ) and os.access(script_path, os.X_OK) :
                    # NOTE: not sure in which scope is best to remove the "#exec " prefix
                    insert_lines = self.__process_command__(line.replace('#exec ', ''))
                    self._lines = self._lines[0:cnt] + insert_lines + self._lines[cnt + 1:]

    def __read_config__(self):
        for line in self._lines:
            if line.find(' ') == -1:
                continue
            command, filename, *arguments = line.split(' ')
            arguments = arguments + ([''] * 2)
            self._config.append([command, filename] + arguments[:2])

    def __process_command__(self, commandline: str):
        script, *arguments = commandline.split(' ')
        arguments = arguments + ([''] * 2)
        script_path = os.path.join(self.path, script)
        result = subprocess.run([script_path] + arguments[:2], stdout=subprocess.PIPE)
        lines = result.stdout.decode('utf-8').splitlines() if result.returncode == 0 else []
        return lines


