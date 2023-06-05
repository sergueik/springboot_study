import click
import sys

@click.group()
def dispatch():
  pass

def show_options(search: str, filename: str, debug: bool):
  click.echo('search = "{}"\nfilename = "{}"\ndebug = {}'.format(search, filename, debug))

# NOTE: parameter names are fixed
# annotation has to be aplied to each qualifying method

@click.command()
@click.option('-d','--debug', is_flag = True, help = 'proceed in debug mode')
@click.option('-s', '--search', prompt = 'Your search argument', help = 'Provide "search" argument')
@click.option('-f', '--filename', prompt = 'Your file argument', help = 'Provide "filename" argument', default = 'dummy.txt')
def method1(search: str, debug: bool, filename: str ):
  click.echo('Method 1 is called')
  show_options(search = search, debug = debug, filename = filename )

@click.command()
@click.option('-d', '--debug', is_flag = True, help = 'proceed in debug mode')
@click.option('-s', '--search', prompt = 'Your search argument', help = 'Provide "search" argument')
@click.option('-f', '--filename', prompt = 'Your file argument', help = 'Provide "filename" argument', default = 'dummy.txt')
def method2(search:  str, filename: str, debug: bool):
  click.echo('Method 2 is called')
  show_options(search = search, debug = debug, filename = filename )

dispatch.add_command(method1)
dispatch.add_command(method2)


if __name__ == '__main__':
  dispatch()

