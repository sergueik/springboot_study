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
        self.read_file()
        self.process_execs()
        self.process_includes()
        return self._lines

    @property
    def config(self):
        self._config = []
        self.read_config()
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

    def read_file(self):
        with open(self.path + '/' + self.filename, mode='r') as file:
            for line in file:
                self._lines.append(line.strip())

    def process_includes(self):
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
                    if os.path.isfile(self.path + '/' + filename):
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

    def process_execs(self):
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
                    insert_lines = self.process_command(line.replace('#exec ', ''))
                    self._lines = self._lines[0:cnt] + insert_lines + self._lines[cnt + 1:]

    def read_config(self):
        for line in self._lines:
            if line.find(' ') == -1:
                continue
            command, filename, *arguments = line.split(' ')
            arguments = arguments + ([''] * 2)
            self._config.append([command, filename] + arguments[:2])

    def process_command(self, commandline: str):
        script, *arguments = commandline.split(' ')
        arguments = arguments + ([''] * 2)
        script_path = os.path.join(self.path, script)
        result = subprocess.run([script_path] + arguments[:2], stdout=subprocess.PIPE)
        lines = result.stdout.decode('utf-8').splitlines() if result.returncode == 0 else []
        return lines


# command line entrance code
help_message = 'usage: service_config.py --path <text> -file <text> [--debug]'


def show_options():
    print(f'debug: {debug}\nconfig_filename: "{config_filename}"\nconfig_path: "{config_path}"')


if __name__ == '__main__':

    try:
        opts, args = getopt.getopt(sys.argv[1:], 'hdp:f:', ['help', 'debug', 'path=', 'file='])
    except getopt.GetoptError as err:
        print(help_message)

    # https://stackoverflow.com/questions/1592565/determine-if-variable-is-defined-in-python
    try:
        opts
    except NameError:
        print(help_message)
        opts = None
        exit()

    global debug
    # TODO:read through environment
    debug = False
    config_path = os.path.curdir
    config_filename = 'data.conf'

    for option, argument in opts:
        if option in ('-d', '--debug'):
            debug = True
        elif option in ('-h', '--help'):
            print(help_message)
            exit()
        elif option in ('-p', '--path'):
            config_path = argument
        elif option in ('-f', '--file'):
            config_filename = argument
        else:
            assert False, 'unhandled option: {}'.format(option)
    if config_path is None or config_filename is None:
        print(help_message)
        exit()
    if debug:
        show_options()

    service_config = ServiceConfig()
    service_config.path = config_path
    service_config.debug = debug
    service_config.filename = config_filename
    # repeat to examine idempotency
    # for cnt in range(3):
    pprint(service_config.lines)
    pprint(service_config.config)
