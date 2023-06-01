import getopt
import sys

def debug_print(line: str):
  if debug != None and debug != False:
    print('DEBUG: "{}"'.format(line))

def operation():
  debug_print('message')
  pass

def show_options(search: str, filename: str, debug: bool):
  print('search = "{}"\nfilename = "{}"\ndebug = {}'.format(search, filename, debug))

def method(search: str, debug: bool, filename: str ):
  print('Method 1 is called')
  show_options(search = search, debug = debug, filename = filename )
  operation()


if __name__ == '__main__':
  try:
    # invoke directly
    method(search = 'google', debug = True, filename = 'dummy.txt')
  except NameError as e:
    # the debug saves
    print('Exception (ignored):\n{}'.format(e))
    # name 'debug' is not defined
    pass
