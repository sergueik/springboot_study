import click
import getopt
import sys

@click.group()
def dispatch():
  pass

def debug_print(line: str):
  if global_debug != None and global_debug != False:
    print('DEBUG: "{}"'.format(line))

def operation():
  debug_print('message')
  pass

def show_options(search: str, filename: str, debug: bool):
  click.echo('search = "{}"\nfilename = "{}"\ndebug = {}'.format(search, filename, debug))

# NOTE: parameter names are fixed
# annotation has to be aplied to each qualifying method

@click.command()
@click.option('-d','--debug', is_flag = True, help = 'proceed in debug mode')
@click.option('-s', '--search', prompt = 'Your search argument', help = 'Provide "search" argument')
@click.option('-f', '--filename', prompt = 'Your file argument', help = 'Provide "filename" argument', default = 'dummy.txt')
def method(search: str, debug: bool, filename: str ):
  global global_debug 
  global_debug = debug
  click.echo('Method 1 is called')
  show_options(search = search, debug = debug, filename = filename )
  operation()

dispatch.add_command(method)


if __name__ == '__main__':
  try:
    # use click to run
    dispatch()
  except NameError as e:
    print('Exception (ignored):\n{}'.format(e))
    # name 'debug' is not defined
    pass


