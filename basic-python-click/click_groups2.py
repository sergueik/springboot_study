import click
import getopt
import sys

@click.group()
def dispatch():
  pass

def show_options(debug: bool,apple = None, banana = None, filename = None ):
  click.echo('apple = "{}"\nbanana = "{}"\ndebug = {}'.format(apple, banana, debug))

# NOTE: parameter names are fixed
# annotation has to be aplied to each qualifying method

@click.command()
@click.option('-d','--debug', is_flag = True, help = 'proceed in debug mode')
@click.option('-a', '--apple', prompt = 'Your apple argument', help = 'Provide "apple" argument')
def apple(apple: str, debug: bool ):
  click.echo('Method 1 is called')
  show_options(apple = apple, debug = debug)

@click.command()
@click.option('-d', '--debug', is_flag = True, help = 'proceed in debug mode')
@click.option('-b', '--banana', prompt = 'Your banana argument', help = 'Provide "banana" argument')
@click.option('-f', '--filename', prompt = 'Your file argument', help = 'Provide "filename" argument', default = 'dummy.txt')
def banana(banana:  str, debug: bool, filename: str):
  click.echo('Method 2 is called')
  show_options(banana = banana, debug = debug, filename = filename )

dispatch.add_command(apple)
dispatch.add_command(banana)


if __name__ == '__main__':
  dispatch()

