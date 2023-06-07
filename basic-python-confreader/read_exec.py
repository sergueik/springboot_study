import getopt
import sys
import os
import re
import subprocess
from pprint import pprint

def exec_process(commandline: str):
  command, *arguments = commandline.split(' ')
  arguments = arguments + ( [''] * 2 )
  if debug:
    print('running command: "{}"'.format(command))
  result = subprocess.run([ command ] + arguments[:2] , stdout = subprocess.PIPE)
  lines = result.stdout.decode('utf-8').splitlines() if result.returncode == 0 else []
  return lines

# command line entrance code

def show_options():
  print(f'debug: {debug}\ncommand: "{command}"')


if __name__ == '__main__':

  try:
    opts, args = getopt.getopt(sys.argv[1:], 'hdc:', ['help', 'debug', 'command=' ])
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
  # TODO: read through envinonment?
  debug = False
  command = 'ls -l .'

  for option, argument in opts:
    if option in ('-d', '--debug'):
      debug = True
    elif option in ('-h', '--help'):
      print(help_message)
      exit()
    elif option in ('-c', '--command'):
      command = argument
    else:
      assert False, 'unhandled option: {}'.format(option)
  if command == None:
    print(help_message)
    exit()
  if debug:
    show_options()

  result = exec_process(command)
  pprint(result)
  for cnt in range(len(result)):
    line = result[cnt]
    if debug:
      print(type(line))
    if line.find(' ') == -1:
      continue
