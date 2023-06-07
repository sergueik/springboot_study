import getopt
import sys
import os
import re
import subprocess
from pprint import pprint

class ServiceConfig(object):
# NOTE: the 'object' super class should be named in all lower case:

  def __init__(self, filename = None, path = None, debug = False):
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
    return self._lines

  @property
  def config(self):
    return self._config

  @filename.setter
  def filename(self, value):
    self._filename = value

  @path.setter
  def path(self, value):
    self._path = value

  # NOTE: can not decorate setter without decorating @property as well
  @debug.setter
  def debug(self, value):
    self._debug = value

  # methods

  def read_file(self):
    with open(self.path + '/'+ self.filename, mode = 'r') as file:
      for line in file:
        self._lines.append(line.strip())

  def process_includes(self):
    patt= re.compile('^#include ')
    # insert
    scanned = False
    recursion_control_cnt = 0
    while not scanned:
      scanned = True
      for cnt in range(len(self._lines)):

        line = self._lines[cnt]
        if line.find(' ') == -1:
          continue

        if patt.match(line) != None:
          if self._debug:
            print('include detected in line {} "{}"'.format(cnt, line))
          _, filename, *_ = line.split(' ')
          if os.path.isfile(self.path + '/' + filename ):
            if self._debug:
              print('valid include filename: "{}"'.format(filename))
            s_c = ServiceConfig()
            s_c.path = self.path
            s_c.filename = filename
            s_c.read_file()
            insert_lines = s_c.lines
            if self._debug:
              print('including lines: {}'.format('\n'.join(insert_lines)))
            scanned = False
            recursion_control_cnt = recursion_control_cnt + 1
            if recursion_control_cnt > 5 :
              # abort recursion which went too far
              scanned = True

            # NOTE: insert lines is tricky inside the range loop
            self._lines = self._lines[0:cnt] + insert_lines + self._lines[cnt + 1:]
            if self._debug:
              pprint(self._lines)

  def process_execs(self):
    patt= re.compile('^#exec ')
    # insert
    for cnt in range(len(self._lines)):

      line = self._lines[cnt]
      # NOTE: TypeError: a bytes-like object is required, not 'str'
      #
      if self._debug:
        print(type(line))
      if line.find(' ') == -1:
        continue

      if patt.match(line) != None:
        if self._debug:
          print('include detected in line {} "{}"'.format(cnt, line))
        _, filename, *_ = line.split(' ')
        if os.path.isfile(self.path + '/' + filename ):
          if self._debug:
            print('valid include filename: "{}"'.format(filename))
          # NOTE: not sure when to remove the "#exec " prefix
          insert_lines = self.process_exec(line.replace('#exec ', ''))
          if self._debug:
            print('including lines: {}'.format('\n'.join(insert_lines)))

          # NOTE: insert lines is tricky inside the range loop
          self._lines = self._lines[0:cnt] + insert_lines + self._lines[cnt + 1:]
          if self._debug:
            pprint(self._lines)


  def tokenize_lines(self):
    for line in self._lines:
      if line.find(' ') == -1:
        continue
        # alternatively, handle ValueError: not enough values to unpack

      command, filename, *arguments = line.split(' ')
      arguments = arguments + ( [''] * 2 )
      self._config.append([ command, filename ] + arguments[:2] )

  def process_exec(self, commandline: str):
    command, *arguments = commandline.split(' ')
    arguments = arguments + ( [''] * 2 )
    command = self._path + '/' + command
    if self._debug:
      print('running command: "{}"'.format(command))
    result = subprocess.run([ command ] + arguments[:2] , stdout = subprocess.PIPE)
    lines = result.stdout.decode('utf-8').splitlines() if result.returncode == 0 else []
    return lines

# command line entrance code
help_message = 'usage: service_config.py --path <text> -file <text> [--debug]'

def show_options():
  print(f'debug: {debug}\nconfig_filename: "{config_filename}"\nconfig_path: "{config_path}"')

if __name__ == '__main__':

  try:
    opts, args = getopt.getopt(sys.argv[1:], 'hdp:f:', ['help', 'debug', 'path=', 'file=' ])
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
  # TODO:read through envinonment
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
  if config_path == None or config_filename == None:
    print(help_message)
    exit()
  if debug:
    show_options()

  s_c = ServiceConfig()
  s_c.path = config_path
  s_c.debug = debug
  s_c.filename = config_filename

  s_c.read_file()
  # NOTE: at this point no #include's are processed
  # pprint(s_c.lines)

  # TODO: move method invocation into the relevant getters
  s_c.process_execs()
  s_c.process_includes()
  s_c.tokenize_lines()

  pprint(s_c.lines)
  pprint(s_c.config)
