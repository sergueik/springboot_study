import getopt
import sys


def debug_print(line: str):
  if global_debug != None and global_debug != False:
    print('DEBUG: "{}"'.format(line))

def operation():
  debug_print('message')
  pass

def show_options(search: str, filename: str, debug: bool):
  print('search = "{}"\nfilename = "{}"\ndebug = {}'.format(search, filename, debug))

def method(search: str, debug: bool, filename: str ):
  # NOTE: SyntaxError: name 'debug' is parameter and global 
  # global debug 
  global global_debug 
  global_debug = debug
  print('Method 1 is called')
  show_options(search = search, debug = debug, filename = filename )
  operation()


if __name__ == '__main__':
  try:
    # invoke directly
    method(search = 'google', debug = True, filename = 'dummy.txt')
  except NameError as e:
    # the global_debug saves from getting the NameError exception
    print('Exception (ignored):\n{}'.format(e))
    pass
