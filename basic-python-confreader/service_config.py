import getopt
import sys
import os
from pprint import pprint

class ServiceConfig(object):
# NOTE: the 'object' super class should be named in all lower case:
  def __init__(self, filename = None, path = None):
     self._file = filename
     self._path = path
     self._lines = []
     self._config = []

  @property
  def filename(self):
    return self._filename

  @property
  def path(self):
    return self._path

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

  def process(self):
    with open(self.path + '/'+ self.filename, mode = 'r') as file:
      for line in file:
        self._lines.append(line.strip())

  def examine(self):
    for line in self._lines:
      if line.find(' ') == -1:
        continue
        # alternatively, handle ValueError: not enough values to unpack
 
      # TODO: rename to arguments
      command, filename, *variables = line.split(' ')
      variables = variables + ( [''] * 2 )
      # NOTE: cannot do in place
      fields = [ command, filename ]
      fields.extend(variables[:2])
      self._config.append(fields)

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
  debug = True
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
  s_c.filename = config_filename

  s_c.process()
  pprint(s_c.lines)

  s_c.examine()
  pprint(s_c.config)
